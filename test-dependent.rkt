#lang racket/base
(require racket/class
         racket/pretty
         "dependent.rkt")
(provide (all-defined-out))

(pretty-print-columns 100)

;; ============================================================

(define grammar% (LR-mixin grammar-base%))

;; ============================================================

(eprintf "\nExample 1:\n")
(define g1
  (DGrammar
   #:start Sent
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
;; Example with LR0 "virtual" accept state
(define g2
  (DGrammar
   #:start Expr
   [Expr (atom)
         (lparen Expr op Expr rparen)]))
(define gg2 (new grammar% (g g2)))
(send gg2 print)

(define s2a '((lparen) (atom 5) (op +) (atom 6) (rparen)))

;; --------------------

(eprintf "\nExample 3:\n")
(define g3
  (DGrammar
   #:start A
   [A (x Y Z)]
   [Y () (y Y)]
   [Z (y)]))
(define gg3 (new grammar% (g g3)))
(send gg3 print)

(define s3a '((x) (y) (y) (y) (y)))

;; --------------------

(eprintf "\nExample 4:\n")
;; Example with LR0 "true" accept state
(define g4
  (DGrammar
   #:start A
   [A (a) (A b)]))
(define gg4 (new grammar% (g g4)))
(send gg4 print)

(define s4a '((a) (b) (b) (b)))

;; --------------------

(eprintf "\nExample 5:\n")
;; Example with LR0 shift/reduce conflict
(define g5
  (DGrammar
   #:start E
   [E (a) (E op E)]))
(define gg5 (new grammar% (g g5)))
(send gg5 print)

(define s5a '((a) (op) (a) (op) (a)))

;; --------------------

(eprintf "\nExample 6:\n")
;; Example with LR0 reduce/reduce conflict, solved by lookahead
(define g6
  (DGrammar
   #:start A
   [A (B x) (C y)]
   [B (a)]
   [C (a)]))
(define gg6 (new grammar% (g g6)))
(send gg6 print)

(define s6a '((a) (y)))

;; ----------------------------------------
