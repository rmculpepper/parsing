#lang racket/base
(require racket/class
         "grammar.rkt"
         "ll1-parser.rkt")
(provide (all-defined-out))

;; ============================================================

(define grammar% (LL1-mixin grammar-base%))

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

;; --------------------

(eprintf "\nExample 4:\n")
(define g4 (Grammar A [A (a) (A b)]))
(define gg4 (new grammar% (g g4)))
(send gg4 print)

(define s4a '((a) (b) (b) (b)))

;; --------------------

(eprintf "\nExample 5:\n")
(define g5
  (Grammar
   E
   [E (a) (E op E)]))
(define gg5 (new grammar% (g g5)))
(send gg5 print)

(define s5a '((a) (op) (a) (op) (a)))

;; ----------------------------------------

(random-seed 17)
;; (printf "Generating corpora\n")
(define corpus1 (begin #;time (send gg1 generate-corpus #e1e2)))
(define corpus2 (begin #;time (send gg2 generate-corpus #e5e1)))
(define corpus3 (begin #;time (send gg3 generate-corpus #e1e2)))
