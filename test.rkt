#lang racket/base
(require racket/class
         "dependent.rkt")
(provide (all-defined-out))

(define (mktz toks)
  (define tok* (case-lambda [(t) (tok t t)] [(t v) (tok t v)]))
  (peeking-tokenizer
   (lambda (_p? _k _a)
     (if (pair? toks) (begin0 (apply tok* (car toks)) (set! toks (cdr toks))) EOF-tok))))

;; ============================================================

(eprintf "\nExample 1:\n")
(define-grammar g1
  #:start Sent
  [Sent [(Clause) #:auto]
        [(Clause conj Sent) #:auto]]
  [Clause [(Nphr Vphr) #:auto]]
  [Nphr [(noun) #:> (list $1)]
        [(adj Nphr) #:> (cons $1 $2)]]
  [Vphr [(verb MaybeObj) #:auto]
        [(adv Vphr) #:auto]]
  [MaybeObj [() null]
            [(Nphr) #:> $1]])
(define gg1 (lr-parser #:grammar g1))
(send gg1 print)

(define s1a '((adj) (adj) (noun) (verb) (adj) (noun) (conj) (noun) (adv) (verb)))
(send gg1 parse (mktz s1a))

;; --------------------

(eprintf "\nExample 2:\n")
;; Example with LR0 "virtual" accept state
(define-grammar g2
  #:start Expr
  [Expr [(atom) #:> $1]
        [(lparen Expr op Expr rparen) (list $2 $3 $4)]])
(define gg2 (lr-parser #:grammar g2))
(send gg2 print)

(define s2a '((lparen) (atom 5) (op +) (atom 6) (rparen)))
(send gg2 parse (mktz s2a))

;; --------------------

(eprintf "\nExample 3:\n")
(define-grammar g3
  #:start A
  [A [(x Y Z) #:auto]]
  [Y [() #:auto]
     [(y Y) #:auto]]
  [Z [(y) #:auto]])
(define gg3 (lr-parser #:grammar g3))
(send gg3 print)

(define s3a '((x) (y) (y) (y) (y)))
;; This example has LR(1) conflict.
;;(send gg3 parse (mktz s3a))

;; --------------------

(eprintf "\nExample 4:\n")
;; Example with LR0 "true" accept state
(define-grammar g4
  #:start A
  [A [(a) #:auto]
     [(A b) #:auto]])
(define gg4 (lr-parser #:grammar g4))
(send gg4 print)

(define s4a '((a) (b) (b) (b)))
(send gg4 parse (mktz s4a))

;; --------------------

(eprintf "\nExample 5:\n")
;; Example with LR0 (and SLR?) shift/reduce conflict
(define-grammar g5
  #:start E
  [E [(a) #:> $1]
     [(E op E) #:> (list $1 $2 $3)]])
(define gg5 (lr-parser #:grammar g5))
(send gg5 print)

(define s5a '((a) (op) (a) (op) (a)))
(send gg5 parse (mktz s5a))

;; --------------------

(eprintf "\nExample 6:\n")
;; Example with LR0 reduce/reduce conflict, solved by lookahead
(define-grammar g6
  #:start A
  [A [(B x) #:auto]
     [(C y) #:auto]]
  [B [(a) #:auto]]
  [C [(a) #:auto]])
(define gg6 (lr-parser #:grammar g6))
(send gg6 print)

(define s6a '((a) (y)))
(send gg6 parse (mktz s6a))

;; ----------------------------------------

#|
(random-seed 17)
;; (printf "Generating corpora\n")
(define corpus1 (begin #;time (send gg1 generate-corpus #e1e2)))
(define corpus2 (begin #;time (send gg2 generate-corpus #e5e1)))
(define corpus3 (begin #;time (send gg3 generate-corpus #e1e2)))
|#
