#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     syntax/parse
                     "private/grammar-rep.rkt"
                     "util/datum-to-expr.rkt"
                     "private/lr-analysis.rkt")
         racket/class
         racket/lazy-require
         "private/common.rkt"
         "private/syntax.rkt"
         "private/lr-runtime.rkt")
(provide (all-defined-out)
         (all-from-out "private/common.rkt")
         define-grammar)

(lazy-require
 ["private/lr-analysis.rkt" (make-LR)])

(begin-for-syntax
  (define (make-parser-expr g mode)
    (define pg (make-LR g mode))
    (define pstates (send pg get-pstates))
    (define vals-expr (datum->expression (send pg get-vals) (lambda (v) (if (syntax? v) v #f))))
    #`(make-lr-parser (quote #,pstates) #,vals-expr (quote #,g) (quote #,mode))))

(define-syntax (lr-parser stx)
  (define-syntax-class mode
    (pattern #:lr0 #:attr mode 'lr0)
    (pattern #:slr1 #:attr mode 'slr1)
    (pattern #:lalr1 #:attr mode 'lalr1))
  (syntax-parse stx
    [(_ (~optional la:mode) #:start start def ...)
     (make-parser-expr (parse-grammar #'start #'(def ...) #:context stx)
                       (or (attribute la.mode) 'lalr1))]
    [(_ (~optional la:mode) #:grammar (~var g (static grammar? "grammar")))
     (make-parser-expr (attribute g.value)
                       (or (attribute la.mode) 'lalr1))]))

#;
(define-syntax (lalr-parser stx)
  (syntax-parse stx
    [(_ #:grammar (~var g (static grammar? "grammar")))
     (make-parser-expr (attribute g.value) 'lalr1)]))

(define lr-parser%
  (class object%
    (init-field pstates vals g mode)
    (super-new)
    (define/public (parse get-token)
      (lr-parse pstates vals get-token))
    (define/public (parse* get-token)
      (glr-parse pstates vals get-token #:mode 'first-done))
    (define/public (print)
      (define rt (make-LR g mode))
      (send rt print))
    (define/public (reify-lr0)
      (define rt (make-LR g mode))
      (send rt reify-lr0))
    ))

(define (make-lr-parser pstates vals g mode)
  (new lr-parser% (pstates pstates) (vals vals) (g g) (mode mode)))
