#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     (rename-in syntax/parse [attribute $])
                     "grammar-rep.rkt"
                     "../util/datum-to-expr.rkt"
                     "lr-analysis.rkt")
         racket/class
         racket/lazy-require
         "common.rkt"
         "syntax.rkt"
         "lr-runtime.rkt"
         "glr-runtime.rkt")
(provide (all-defined-out))

(lazy-require
 ["lr-analysis.rkt" (make-LR)])

(begin-for-syntax
  (define-syntax-class mode
    (pattern #:lr0 #:attr mode 'lr0)
    (pattern #:slr1 #:attr mode 'slr1)
    (pattern #:lalr1 #:attr mode 'lalr1))
  (define (make-parser-expr g+ mode)
    (define pg (make-LR g+ mode))
    (define pstates (send pg get-pstates))
    (define vals-expr (send pg get-vals))
    #`(make-lr-parser (quote #,pstates) #,vals-expr (quote #,g+) (quote #,mode))))

(define-syntax (lr-parser stx)
  (syntax-parse stx
    [(_ (~optional la:mode) spec:grammar+start+end)
     (make-parser-expr ($ spec.ast) (or ($ la.mode) 'lalr1))]))

(define lr-parser%
  (class object%
    (init-field pstates vals g+ mode)
    (super-new)
    (define/public (parse get-token)
      (lr-parse pstates vals get-token))
    (define/public (parse* get-token)
      (glr-parse pstates vals get-token #:mode 'first-done))
    (define/public (print)
      (define rt (make-LR g+ mode))
      (send rt print))
    (define/public (reify-lr0)
      (define rt (make-LR g+ mode))
      (send rt reify-lr0))
    ))

(define (make-lr-parser pstates vals g+ mode)
  (new lr-parser% (pstates pstates) (vals vals) (g+ g+) (mode mode)))
