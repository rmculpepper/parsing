#lang racket/base
(require (for-syntax racket/base
                     racket/match
                     racket/class
                     syntax/parse
                     "../util/datum-to-expr.rkt"
                     "grammar-rep.rkt"
                     "base-analysis.rkt"
                     "unger-common.rkt")
         racket/class
         racket/lazy-require
         "common.rkt"
         "syntax.rkt"
         "unger-runtime.rkt")
(provide unger-parser)

;; ============================================================

(module analysis racket/base
  (require racket/class
           racket/match
           "grammar-rep.rkt"
           "base-analysis.rkt"
           "unger-common.rkt")
  (provide (all-defined-out))

  (define (make-unger-grammar g)
    (new unger-grammar% (g g)))

  (define unger-grammar%
    (class grammar-base%
      (super-new)
      (inherit-field start defs nt-h)
      (inherit nt? nt-minlen)

      ;; ----------------------------------------

      (define nt-index-h
        (for/hash ([def (in-list defs)] [index (in-naturals)])
          (values (def-nt def) index)))
      (define/public (nt-index nt) (hash-ref nt-index-h nt))
      (define/private (ntelem-index e) (nt-index (ntelem-nt e)))

      ;; ----------------------------------------

      (define/public (get-unger-start)
        (nt-index start))

      (define/public (get-unger-nts)
        (list->vector
         (for/list ([d (in-list defs)] [index (in-naturals)])
           (match-define (def nt prods) d)
           (unger-nt nt
                     (for/list ([p (in-list prods)]) (prod->unger-prod p))
                     (nt-minlen nt)))))

      (define/private (prod->unger-prod p)
        (match-define (prod nt index item action) p)
        (prod nt index (item->unger-item (vector->list item)) action))

      (define/private (item->unger-item item)
        (define none (unger-nts null))
        (define (init-loop item)
          (cond [(and (pair? item) (telem? (car item)))
                 (unger-split none (telem-t (car item)) (init-loop (cdr item)))]
                [else (final-loop (reverse item))]))
        (define (final-loop item) ;; item is reversed
          (cond [(and (pair? item) (telem? (car item)))
                 (unger-split (final-loop (cdr item)) (telem-t (car item)) none)]
                [#f (left-loop (reverse item) null)]
                [else (right-loop item null)]))
        (define (right-loop item acc) ;; item is reversed
          (cond [(null? item) (unger-nts acc)]
                [(telem? (car item))
                 (unger-split (right-loop (cdr item) null)
                              (telem-t (car item))
                              (unger-nts acc))]
                [else (right-loop (cdr item) (cons (ntelem-index (car item)) acc))]))
        (define (left-loop item acc)
          (cond [(null? item) (unger-nts (reverse acc))]
                [(telem? (car item))
                 (unger-split (unger-nts (reverse acc))
                              (telem-t (car item))
                              (left-loop (cdr item) null))]
                [else (left-loop (cdr item) (cons (ntelem-index (car item)) acc))]))
        (init-loop item)))))
(require (for-syntax 'analysis))

(lazy-require
 [(submod "." analysis) (make-unger-grammar)])

(define-syntax (unger-parser stx)
  (define (k g)
    (define ug (new unger-grammar% (g g)))
    (define vals-expr (datum->expression (grammar-vals g) (lambda (v) (if (syntax? v) v #f))))
    (with-syntax ([g g]
                  [start (send ug get-unger-start)]
                  [nts (send ug get-unger-nts)]
                  [vals-expr vals-expr])
      #'(make-unger-parser (quote start) (quote nts) vals-expr (quote g))))
  (syntax-parse stx
    [(_ #:grammar (~var g (static grammar? "grammar")))
     (k (attribute g.value))]))

(define unger-parser%
  (class object%
    (init-field start nts vals g)
    (super-new)
    (define/public (parse tokens)
      (unger-parse start nts vals tokens))
    (define/public (generate-corpus n)
      (send (make-unger-grammar g) generate-corpus n))
    ))

(define (make-unger-parser start nts vals g)
  (new unger-parser% (start start) (nts nts) (vals vals) (g g)))
