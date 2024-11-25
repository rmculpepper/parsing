;; Copyright 2019-2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/class
         racket/pretty
         racket/match
         "grammar-rep.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

;; ============================================================

(define base-analysis-done%
  (class object%
    (init-field summary-data)
    (super-new)

    (field [g+ (hash-ref summary-data 'grammar+ #f)]
           [nt-nullable-h (hash-ref summary-data 'nullable #f)]
           [nt-minlen-h (hash-ref summary-data 'min-length #f)]
           [nt-first-h (hash-ref summary-data 'first #f)]
           [nt-follow-h (hash-ref summary-data 'follow #f)])

    (define/public (print)
      (when nt-nullable-h
        (printf "Nullable: ~s\n"
                (for/list ([(nt n?) (in-hash nt-nullable-h)] #:when n?) nt)))
      (when nt-minlen-h
        (printf "Min length: ~s\n"
                (for/list ([(nt len) (in-hash nt-minlen-h)]) (list nt len))))
      (when nt-first-h
        (printf "First: ~s\n"
                (for/list ([(nt fs) (in-hash nt-first-h)])
                  (list nt ': (map telem-t fs)))))
      (when nt-follow-h
        (printf "Follow: ~s\n"
                (for/list ([(nt fs) (in-hash nt-follow-h)])
                  (list nt ': (for/list ([f fs]) (if (telem? f) (telem-t f) f)))))))
    ))

;; ============================================================

(define LL1-done%
  (class base-analysis-done%
    (inherit-field summary-data)
    (super-new)

    (field [conflicts (hash-ref summary-data 'conflicts #f)])

    (define/override (print)
      (super print)
      (when conflicts
        (printf "LL(1) Conflicts:\n")
        (pretty-print conflicts)))
    ))

;; ============================================================

(define LR-done%
  (class base-analysis-done%
    (inherit-field summary-data)
    (super-new)

    (field [states (hash-ref summary-data 'states #f)]
           [pstates (hash-ref summary-data 'pstates #f)]
           [lookahead-mode (hash-ref summary-data 'lookahead-mode #f)]
           [conflicts (hash-ref summary-data 'conflicts #f)])

    (define/override (print)
      (when (and states pstates)
        (print-states))
      (super print)
      (when conflicts
        (printf "~a Conflicts: ~v\n"
                (case lookahead-mode
                  [(lr0) "LR(0)"] [(slr1) "SLR(1)"] [(lalr1) "LALR(1)"] [else "??"])
                conflicts)))

    (define/public (print-states [edges? #t])
      (for ([k (vector-length pstates)])
        (print-state k edges?)))

    (define/public (print-state i [edges? #t])
      (define st (vector-ref pstates i))
      (define state (vector-ref states i))
      (match-define (pstate _ label tr shift reduce goto reduce-lookahead) st)
      (printf "State #~s:\n" i)
      (for ([lp (in-list state)])
        (printf "  ~s\n" lp))
      (when edges?
        (printf "  with transitions:\n")
        (for ([(t next-st) (in-hash shift)])
          (printf "  ~v -> shift #~v\n" t next-st))
        (cond [(hash? reduce-lookahead)
               (for* ([(la reds) (in-hash reduce-lookahead)]
                      [red (in-list reds)])
                 (match-define (reduction nt index _ _ _) red)
                 (printf "  ~v -> reduce ~v (~v)\n" la nt index))]
              [else
               (for ([red (in-list reduce)])
                 (match-define (reduction nt index _ _ _) red)
                 (printf "  -> reduce ~v (~v)\n" nt index))])
        (for ([(nt next-st) (in-hash goto)])
          (printf "  ~v -> goto #~v\n" nt next-st))))

    ))
