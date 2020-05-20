#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     (rename-in syntax/parse [attribute $])
                     "lr-analysis.rkt")
         racket/class
         racket/promise
         racket/lazy-require
         "syntax.rkt"
         "analysis-runtime.rkt"
         "lr-runtime.rkt"
         "glr-runtime.rkt")
(provide (all-defined-out))

(begin-for-syntax
  (define-syntax-class mode
    (pattern #:lr0 #:attr mode 'lr0)
    (pattern #:slr1 #:attr mode 'slr1)
    (pattern #:lalr1 #:attr mode 'lalr1))
  (define (make-parser-expr g+ mode)
    (define pg (make-LR g+ mode))
    (define pstates (send pg get-pstates))
    (define vals-expr (send pg get-vals))
    #`(make-lr-parser (quote #,pstates) #,vals-expr (quote #,mode)
                      (quote #,(send pg get-summary-data)))))

(define-syntax (lr-parser stx)
  (syntax-parse stx
    [(_ (~optional la:mode) spec:grammar+start+end)
     (make-parser-expr ($ spec.ast) (or ($ la.mode) 'lalr1))]))

(define lr-parser%
  (class object%
    (init-field pstates vals mode)
    (init summary-data)
    (super-new)

    (define/public (parse tz)
      (lr-parse pstates vals tz))
    (define/public (parse* tz)
      (glr-parse pstates vals tz #:mode 'first-done))

    (define rt (delay (new LR-done% (summary-data summary-data))))
    (define/public (print)
      (send (force rt) print))
    (define/public (reify-lr0)
      (send (force rt) reify-lr0))
    ))

(define (make-lr-parser pstates vals mode summary-data)
  (new lr-parser% (pstates pstates) (vals vals) (mode mode) (summary-data summary-data)))
