#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "util/misc.rkt"
         "private/grammar-rep.rkt"
         "private/common.rkt"
         "private/syntax.rkt"
         "private/base-analysis.rkt"
         "private/lr-analysis.rkt"
         "private/lr-runtime.rkt")
(provide (all-defined-out)
         (all-from-out "private/common.rkt")
         (all-from-out "private/syntax.rkt")
         (all-from-out "private/base-analysis.rkt")
         (all-from-out "private/lr-analysis.rkt"))

(define lr-parser%
  (class object%
    (init-field g)
    (super-new)

    (define pg (new LR% (g g)))

    (define/public (parse get-token)
      (lr-parse (send pg get-pstates) (send pg get-vals) get-token))

    (define/public (print)
      (send pg print))))
