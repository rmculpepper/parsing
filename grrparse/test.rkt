#lang racket/base
(require racket/class
         racket/match
         "main.rkt"
         "private/common.rkt")
(provide (all-defined-out)
         (all-from-out "main.rkt"))

(define PRINT? #f)

(module util racket/base
  (require racket/match "main.rkt")
  (provide (all-defined-out))

  (define (make-toks vs)
    (for/list ([v (in-list vs)] [i (in-naturals)])
      (match v [(list t) (token t t i i)] [(list t v) (token t v i i)])))

  (define (mktz vs)
    (define toks (make-toks vs))
    (define (get-token _k _a)
      (if (pair? toks) (begin0 (car toks) (set! toks (cdr toks))) (token 'EOF)))
    (tokenizer get-token void)))

(require 'util)

;; ============================================================

(eprintf "\nExample 1:\n")
(define-grammar g1
  [Sent [(Clause) #:auto]
        [(Clause conj Sent) #:auto]]
  [Clause [(Nphr Vphr) #:auto]]
  [Nphr [(noun) #:> (list $1)]
        [(adj Nphr) #:> (cons $1 $2)]]
  [Vphr [(verb MaybeObj) #:auto]
        [(adv Vphr) #:auto]]
  [MaybeObj [() null]
            [(Nphr) #:> $1]])
(define gg1 (lr-parser #:grammar g1 #:start Sent))
(when PRINT? (send gg1 print))

(define s1a '((adj) (adj) (noun) (verb) (adj) (noun) (conj) (noun) (adv) (verb)))
(send gg1 parse (mktz s1a))
(send gg1 parse* (mktz s1a))

;; --------------------

(eprintf "\nExample 2:\n")
;; Example with implicit end
(define-grammar g2
  [Expr [(atom) #:> $1]
        [(lparen Expr op Expr rparen) (list $2 $3 $4)]])
(define gg2 (lr-parser #:grammar g2 #:start Expr #:implicit-end))
(when PRINT? (send gg2 print))

(define lg2 (ll1-parser #:grammar g2 #:start Expr #:implicit-end))
(when PRINT? (send lg2 print))

(define s2a '((lparen) (atom 5) (op +) (atom 6) (rparen)))
(send gg2 parse (mktz s2a))
(send gg2 parse* (mktz s2a))
(send lg2 parse (mktz s2a))

;; --------------------

(eprintf "\nExample 3:\n")
(define-grammar g3
  [A [(x Y Z) #:auto]]
  [Y [() #:auto]
     [(y Y) #:auto]]
  [Z [(y) #:auto]])
(define gg3 (lr-parser #:grammar g3 #:start A))
(when PRINT? (send gg3 print))

(define s3a '((x) (y) (y) (y) (y)))
(printf "-- LR(1) conflict\n") ;; skip parse
(send gg3 parse* (mktz s3a))

;; --------------------

(eprintf "\nExample 4:\n")
;; Example with LR0 "true" accept state
(define-grammar g4
  [A [(a) #:auto]
     [(A b) #:auto]])
(define gg4 (lr-parser #:grammar g4 #:start A))
(when PRINT? (send gg4 print))

(define s4a '((a) (b) (b) (b)))
(send gg4 parse (mktz s4a))
(send gg4 parse* (mktz s4a))

;; --------------------

(eprintf "\nExample 5:\n")
;; Example with LR0 (and LR1?) shift/reduce conflict
(define-grammar g5
  [E [(a) #:> $1]
     [(E op E) #:> (list $1 $2 $3)]])
(define gg5 (lr-parser #:grammar g5 #:start E))
(when PRINT? (send gg5 print))

(define s5a '((a) (op) (a) (op) (a)))
(send gg5 parse (mktz s5a))
(send gg5 parse* (mktz s5a))

;; --------------------

(eprintf "\nExample 6:\n")
;; Example with LR0 reduce/reduce conflict, solved by lookahead
(define-grammar g6
  [A [(B x) #:auto]
     [(C y) #:auto]]
  [B [(a) #:auto]]
  [C [(a) #:auto]])
(define gg6 (lr-parser #:grammar g6 #:start A #:implicit-end))
(when PRINT? (send gg6 print))

(define s6a '((a) (y)))
(send gg6 parse (mktz s6a))
(send gg6 parse* (mktz s6a))

;; --------------------

(eprintf "\nExample 7:\n")
;; LALR(1) but not LR(0)
(define-grammar g7
  [S [(E) #:auto]]
  [E [(E minus T) #:auto]
     [(T) #:auto]]
  [T [(number) #:auto]
     [(lparen E rparen) #:auto]])
(define gg7 (lr-parser #:grammar g7 #:start S))
(when PRINT? (send gg7 print))

(define s7a '((number 5) (minus) (number 2) (minus) (number 1)))
(send gg7 parse (mktz s7a))
(send gg7 parse* (mktz s7a))

;; --------------------

(eprintf "\nExample 8:\n")
;; LALR(1) but not SLR(1)
(define-grammar g8
  [S [(A a A b) #:auto]
     [(B b B a) #:auto]]
  [A [(x) #:auto]]
  [B [(x) #:auto]])
(define gg8 (lr-parser #:grammar g8 #:start S))
(when PRINT? (send gg8 print))

(define s8a '((x) (a) (x) (b)))
(send gg8 parse (mktz s8a))
(send gg8 parse* (mktz s8a))

;; --------------------

(eprintf "\nExample 9:\n")
;; LALR(1) but not SLR(1)
(define-grammar g9
  [S [(A a) #:auto]
     [(b A c) #:auto]
     [(d c) #:auto]
     [(b d a) #:auto]]
  [A [(d) #:auto]])
(define gg9 (lr-parser #:grammar g9 #:start S))
(when PRINT? (send gg9 print))

(define s9a '((d) (c)))
(define s9b '((b) (d) (a)))

(send gg9 parse (mktz s9a))
(send gg9 parse* (mktz s9a))

(send gg9 parse (mktz s9b))
(send gg9 parse* (mktz s9b))

;; --------------------

(eprintf "\nExample 10:\n")
;; Example of LL(1) grammar that is not LALR(1)!
;; Ref: https://stackoverflow.com/questions/6487588/#6492798
(define-grammar g10
  [S [(lparen X) #:auto]
     [(E rbracket) #:auto]
     [(F rparen) #:auto]]
  [X [(E rparen) #:auto]
     [(F rbracket) #:auto]]
  [E [(A) #:auto]]
  [F [(A) #:auto]]
  [A [() #:auto]])
(define gg10 (lr-parser #:grammar g10 #:start S))
(when PRINT? (send gg10 print))

(define lg10 (ll1-parser #:grammar g10 #:start S))
(when PRINT? (send lg10 print))

(define s10a '((lparen) (rparen)))
(define s10b '((rparen) (rbracket))) ;; -- BAD, contains extra token!
(define s10c '((rparen)))

(printf "-- not LALR(1)\n") ;; skip parse
(send lg10 parse (mktz s10a))
(send gg10 parse* (mktz s10a))

(send lg10 parse (mktz s10b))
;;(send gg10 parse* (mktz s10b))

(send lg10 parse (mktz s10c))
(send gg10 parse* (mktz s10c))

;; --------------------

(eprintf "\nExample 11:\n")
;; Another example of implicit-end
(define-grammar g11
  [E [(lparen T rparen) #:auto]]
  [T [(atom) #:auto]
     [(T op T) #:auto]
     [(E) #:auto]])
(define gg11 (lr-parser #:grammar g11 #:start E #:implicit-end))
(when PRINT? (send gg11 print))

(define s11a '((lparen) (atom) (op) (atom) (op) (atom) (rparen)))

(send gg11 parse (mktz s11a))
(send gg11 parse* (mktz s11a))

;; ----------------------------------------

(eprintf "\nExample 12:\n")
;; Same as 11, but internal def, not module top level
(define gg12
  (let ()
    (define-grammar g12
      [E [(lparen T rparen) #:auto]]
      [T [(atom) #:auto]
         [(T op T) #:auto]
         [(E) #:auto]])
    (lr-parser #:grammar g11 #:start E #:implicit-end)))
(when PRINT? (send gg12 print))

(send gg12 parse (mktz s11a))
(send gg12 parse* (mktz s11a))


;; ========================================
;; Parameters

(eprintf "\nExample P 1\n")
(define-grammar p1
  [S [(P A) $2]]
  [P [() (list 1 2)]]
  [A #:context [xy]
     [(a) (list xy $1)]])

(define gp1 (lr-parser #:grammar p1 #:start S))
(define ss1a '((a)))

(send gp1 parse (mktz ss1a))
(send gp1 parse* (mktz ss1a))


;; ========================================
;; Disambiguation filters

(eprintf "\nExample F 1\n")
(define-grammar f1
  [S [() null]
     [(A S) (cons $1 $2)]]
  [A [(XS) (if (= (length $1) 1) $1 (filter:reject))]]
  [XS [(x) (list $1)]
      [(x XS) (cons $1 $2)]])
(define fp1 (lr-parser #:grammar f1 #:start S))

(define sf1a '((x) (x) (x) (x)))
;;(send fp1 parse (mktz sf1a))
(send fp1 parse* (mktz sf1a))