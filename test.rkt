#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar.rkt"
         "unger-parser.rkt"
         "ll1-parser.rkt"
         "lr-parser.rkt"
         "util/stream.rkt")
(provide (all-defined-out))

;; ============================================================

(define grammar%
  (LR-mixin (unger-mixin (LL1-mixin grammar-base%))))

;; ============================================================

(eprintf "\nExample 1:\n")
(define g1
  (Grammar
   Sent
   [Sent (Clause)
         (Clause conj Sent)]
   [Clause (Nphr Vphr)]
   [Nphr (noun)
         (adj Nphr)]
   [Vphr (verb MaybeObj)
         (adv Vphr)]
   [MaybeObj () (Nphr)]))
(define gg1 (new grammar% (g g1)))
(send gg1 print)

(define s1a '((adj) (adj) (noun) (verb) (adj) (noun) (conj) (noun) (adv) (verb)))
(s->list (send gg1 unger-parse s1a))

;; --------------------

(eprintf "\nExample 2:\n")
(define g2
  (Grammar
   Expr
   [Expr (atom)
         (lparen Expr op Expr rparen)]))
(define gg2 (new grammar% (g g2)))
(send gg2 print)

(define s2a '((lparen) (atom 5) (op +) (atom 6) (rparen)))
(send gg2 ll1-parse s2a)
(s->list (send gg2 unger-parse s2a))

;; --------------------

(eprintf "\nExample 3:\n")
(define g3
  (Grammar
   A
   [A (x Y Z)]
   [Y () (y Y)]
   [Z (y)]))
(define gg3 (new grammar% (g g3)))
(send gg3 print)

(define s3a '((x) (y) (y) (y) (y)))
(s->list (send gg3 unger-parse s3a))

;; ----------------------------------------

(when #f
  (time (for ([i (in-range #e1e4)])
          (s->list (send gg1 unger-parse s1a))))
  (time (for ([i (in-range #e1e5)])
          (s->list (send gg2 unger-parse s2a))))
  (void))

(when #f
  (random-seed 17)

  (printf "Generating corpora\n")
  (define corpus1 (time (send gg1 generate-corpus #e1e2)))
  (define corpus2 (time (send gg2 generate-corpus #e5e1)))
  (define corpus3 (time (send gg3 generate-corpus #e1e2)))

  (when #f
    (printf "Parsing corpora using Unger parser\n")
    (time (for ([i (in-range #e1e1)])
            (for ([s (in-list corpus1)])
              (s->list (send gg1 unger-parse s)))))
    (time (for ([i (in-range #e1e1)])
            (for ([s (in-list corpus2)])
              (eprintf "parsing (~s) ~v\n" (length s) s)
              (s->list (send gg2 unger-parse s)))))
    (time (for ([i (in-range #e1e1)])
            (for ([s (in-list corpus3)])
              (s->list (send gg3 unger-parse s)))))
    (void)))
