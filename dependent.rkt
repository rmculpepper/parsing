#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     syntax/parse
                     "util/datum-to-expr.rkt"
                     "private/lr-analysis.rkt")
         racket/class
         racket/lazy-require
         "private/common.rkt"
         "private/syntax.rkt"
         "private/lr-runtime.rkt")
(provide (all-defined-out)
         (all-from-out "private/common.rkt"))

(lazy-require
 ["private/lr-analysis.rkt" (make-LR)])

(define-syntax (lr-parser stx)
  (syntax-parse stx
    [(_ #:start start def ...)
     (define g (parse-grammar #'start #'(def ...) #:context stx))
     (define pg (new LR% (g g)))
     (define pstates (send pg get-pstates))
     (define vals-expr (datum->expression (send pg get-vals) (lambda (v) (if (syntax? v) v #f))))
     (with-syntax ([g g]
                   [pstates (send pg get-pstates)]
                   [vals-expr vals-expr])
       #'(make-lr-parser (quote pstates) vals-expr (quote g)))]))

(define lr-parser%
  (class object%
    (init-field pstates vals g)
    (super-new)
    (define/public (parse get-token)
      (lr-parse pstates vals get-token))
    (define/public (print)
      (define rt (make-LR g))
      (send rt print))
    ))

(define (make-lr-parser pstates vals g)
  (new lr-parser% (pstates pstates) (vals vals) (g g)))
