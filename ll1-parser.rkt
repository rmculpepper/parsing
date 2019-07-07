#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar.rkt")
(provide (all-defined-out))

;; ============================================================

(define (LL1-mixin base%)
  (class base%
    (super-new)
    (inherit-field start defs)
    (inherit nt? terminal?
             symbol-nullable? 
             symbol-first item-first
             nt-follow)

    ;; ----------------------------------------

    (define ll1-first-first-conflicts
      (append*
       (for/list ([def defs])
         (define nt (def-nt def))
         (for/fold ([conflicts null] [acc-first null] #:result conflicts)
                   ([item (def-rhss def)] [index (in-naturals)])
           (define overlap (set-intersect acc-first (item-first item)))
           (values (if (set-empty? overlap)
                       conflicts
                       (cons `(first-first ,nt ,index ,overlap) conflicts))
                   (set-union acc-first (item-first item)))))))
    (define ll1-first-follow-conflicts
      (append*
       (for/list ([def defs])
         (define nt (def-nt def))
         (define overlap (set-intersect (symbol-first nt) (nt-follow nt)))
         (cond [(and (symbol-nullable? nt) (not (set-empty? overlap)))
                (list `(first-follow ,nt ,overlap))]
               [else null]))))
    (define ll1-conflicts
      (append ll1-first-first-conflicts ll1-first-follow-conflicts))

    (define ll1-table
      ;; Hash[NT => Hash[TerminalSymbol => (cons Nat Item)]]
      (for/hash ([def defs])
        (define nt (def-nt def))
        (values nt
                (for/fold ([h (hash)]) ([rhs (def-rhss def)] [index (in-naturals)])
                  (define ts
                    (set-union (item-first rhs)
                               (if (symbol-nullable? nt) (nt-follow nt) null)))
                  (for/fold ([h h]) ([t ts]) (hash-set h t (cons index rhs)))))))

    (define/public (ll1-parse toks)
      (define-values (result rest-toks) (ll1-parse* toks))
      (unless (null? rest-toks) (error 'll1-parse "expected EOF, got ~e" rest-toks))
      result)

    (define/public (ll1-parse* toks)
      (let loop ([want start] [input toks])
        (define-values (next-t next-tok rest-input)
          (match input
            [(cons tok1 input2) (values (tok-t tok1) tok1 input2)]
            ['() (values EOF EOF input)]))
        ;; (eprintf "LL1: want ~v, next ~v\n" want next-tok)
        (cond [(nt? want)
               (define state (hash-ref ll1-table want))
               (match (hash-ref state next-t #f)
                 [(cons index rhs)
                  (for/fold ([acc (list index want)] [input input]
                             #:result (values (reverse acc) input))
                            ([sym rhs])
                    (define-values (result rest-input) (loop sym input))
                    (values (cons result acc) rest-input))]
                 [#f (error 'll1-parse "expected ~v, got ~v" want next-t)])]
              [else
               (if (eq? next-t want)
                   (values next-tok rest-input)
                   (error 'll1-parse "expected ~v, got ~v" want next-tok))])))

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
