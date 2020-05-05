#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/set
         racket/pretty
         "grammar-rep.rkt"
         "base-analysis.rkt")
(provide (all-defined-out))

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

;; ============================================================

(define LL1%
  (class grammar-base%
    (super-new)
    (inherit-field g start defs)
    (inherit nt?
             nt-nullable? item-nullable?
             nt-first item-first
             nt-follow)

    (define/public (get-vals) (grammar-vals g))

    (define/public (def->entry d)
      (match-define (def nt rhss) d)
      (define tr
        (let ([telems (set-union (nt-first nt) (if (nt-nullable? nt) (nt-follow nt) null))])
          (telems-consistent-tr 'll1-parser telems)))
      (cons tr
            (for/fold ([h (hash)]) ([p (in-list rhss)])
              (match-define (prod nt index item action) p)
              (define telems
                (set-union (item-first item) (if (item-nullable? item) (nt-follow nt) null)))
              (for/fold ([h h]) ([te (in-list telems)])
                (hash-cons h (telem-t te) p)))))

    (define ll1-table
      ;; Hash[NT => Hash[Terminal => (cons Nat (Listof Prod))]
      (for/fold ([h (hash)]) ([d (in-list defs)])
        (hash-set h (def-nt d) (def->entry d))))

    (define/public (get-table) ll1-table)

    (define ll1-conflicts
      (append
       (append* ;; First/First conflicts
        (for/list ([def defs])
          (define nt (def-nt def))
          (for/fold ([conflicts null] [acc-first null] #:result conflicts)
                    ([p (def-rhss def)] [index (in-naturals)])
            (match-define (prod nt index item action) p)
            (define overlap (set-intersect acc-first (item-first item)))
            (values (if (set-empty? overlap)
                        conflicts
                        (cons `(first-first ,nt ,index ,overlap) conflicts))
                    (set-union acc-first (item-first item))))))
       (append* ;; First/Follow conflicts
        (for/list ([def defs])
          (define nt (def-nt def))
          (define overlap (set-intersect (nt-first nt) (nt-follow nt)))
          (cond [(and (nt-nullable? nt) (not (set-empty? overlap)))
                 (list `(first-follow ,nt ,overlap))]
                [else null])))))

    ;; ----------------------------------------

    (define/override (print)
      (super print)
      (when (pair? ll1-conflicts)
        (printf "LL1 Conflicts:\n")
        (pretty-print ll1-conflicts))
      (when (null? ll1-conflicts)
        (printf "LL1 Table:\n")
        (pretty-print ll1-table)))
    ))

(define (make-LL1 g)
  (new LL1% (g g)))