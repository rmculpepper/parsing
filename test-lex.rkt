#lang racket/base
(require racket/class
         racket/match
         "main.rkt"
         "private/common.rkt"
         "private/lex-rx.rkt")
(provide (all-defined-out)
         (all-from-out "main.rkt"))

(define (words-token-reader words)
  (apply make-token-reader
         #rx"[ \t\r\n]+" (lambda (l s e) #f)
         (let loop ([words words])
           (cond [(null? words) null]
                 [else (list* (regexp (regexp-quote (format "~a" (car words))))
                              (lambda (l s e) (token (car words) (car words)))
                              (loop (cdr words)))]))))

(define (words-lexer words)
  (make-lexer (words-token-reader words)))

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

(define lex1 (words-lexer '(conj noun adj verb adv)))
(define ts1a "adj adj noun verb adj noun conj noun adv verb")
(send gg1 parse (lex1 ts1a))
(send gg1 parse* (lex1 ts1a))

;; --------------------

(eprintf "\nExample 2:\n")
;; Example with implicit end
(define-grammar g2
  [Expr [(atom) #:> $1]
        [(lparen Expr op Expr rparen) (list $2 $3 $4)]])
(define gg2 (lr-parser #:grammar g2 #:start Expr #:implicit-end))
(define lg2 (ll1-parser #:grammar g2 #:start Expr #:implicit-end))

(define lex2
  (make-lexer
   (make-token-reader
    #rx"\\(" (lambda (l s e) 'lparen)
    #rx"\\)" (lambda (l s e) 'rparen)
    #rx"[0-9]+" (lambda (l s e) (token 'atom (string->number l)))
    #rx"[+*]" (lambda (l s e) (token 'op (string->symbol l)))
    #rx"[ \t\r\n]+" (lambda (l s e) #f))))

(define ts2a "(5 + 6)")
(send gg2 parse (lex2 ts2a))
(send gg2 parse* (lex2 ts2a))
(send lg2 parse (lex2 ts2a))

;; Example for error reporting:
;;(define ts2b "(5 + 6*7)")
;;(send gg2 parse* (lex2 ts2b))

;; --------------------

(eprintf "\nExample 3:\n")
(define-grammar g3
  [A [(x Y Z) #:auto]]
  [Y [() #:auto]
     [(y Y) #:auto]]
  [Z [(y) #:auto]])
(define gg3 (lr-parser #:grammar g3 #:start A))

(define lex3 (words-lexer '(x y)))
(define ts3a "xyyyy")
(printf "-- LR(1) conflict\n") ;; skip parse
(send gg3 parse* (lex3 ts3a))

;; --------------------

(eprintf "\nExample 4:\n")
;; Example with LR0 "true" accept state
(define-grammar g4
  [A [(a) #:auto]
     [(A b) #:auto]])
(define gg4 (lr-parser #:grammar g4 #:start A))

(define lex4 (words-lexer '(a b)))
(define ts4a "abbb")
(send gg4 parse (lex4 ts4a))
(send gg4 parse* (lex4 ts4a))

;; --------------------

(eprintf "\nExample 5:\n")
;; Example with LR0 (and LR1?) shift/reduce conflict
(define-grammar g5
  [E [(atom) #:> $1]
     [(E op E) #:> (list $1 $2 $3)]])
(define gg5 (lr-parser #:grammar g5 #:start E))

(define lex5
  (make-lexer
   (make-token-reader
    #rx"\\(" (lambda (l s e) 'lparen)
    #rx"\\)" (lambda (l s e) 'rparen)
    #rx"[0-9]+" (lambda (l s e) (token 'atom (string->number l)))
    #rx"[+*]" (lambda (l s e) (token 'op (string->symbol l)))
    #rx"[ \t\r\n]+" (lambda (l s e) #f))))

(define ts5a "1 + 2 *3")
(send gg5 parse (lex5 ts5a))
(send gg5 parse* (lex5 ts5a))

;; --------------------

(eprintf "\nExample 6:\n")
;; Example with LR0 reduce/reduce conflict, solved by lookahead
(define-grammar g6
  [A [(B x) #:auto]
     [(C y) #:auto]]
  [B [(a) #:auto]]
  [C [(a) #:auto]])
(define gg6 (lr-parser #:grammar g6 #:start A #:implicit-end))

(define lex6 (words-lexer '(a y)))
(define ts6a "ay")
(send gg6 parse (lex6 ts6a))
(send gg6 parse* (lex6 ts6a))

;; --------------------

(eprintf "\nExample 7:\n")
;; LALR(1) but not LR(0)
(define-grammar g7
  [S [(E) #:auto]]
  [E [(E op T) #:auto]
     [(T) #:auto]]
  [T [(atom) #:auto]
     [(lparen E rparen) #:auto]])
(define gg7 (lr-parser #:grammar g7 #:start S))

(define lex7 lex5)

(define ts7a "5 + 2 * 1")
(send gg7 parse (lex7 ts7a))
(send gg7 parse* (lex7 ts7a))

;; --------------------

(eprintf "\nExample 8:\n")
;; LALR(1) but not SLR(1)
(define-grammar g8
  [S [(A a A b) #:auto]
     [(B b B a) #:auto]]
  [A [(x) #:auto]]
  [B [(x) #:auto]])
(define gg8 (lr-parser #:grammar g8 #:start S))

(define lex8 (words-lexer '(x a b)))
(define ts8a "xaxb")
(send gg8 parse (lex8 ts8a))
(send gg8 parse* (lex8 ts8a))

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

(define lex9 (words-lexer '(a b c d)))
(define ts9a "dc")
(define ts9b "bda")

(send gg9 parse (lex9 ts9a))
(send gg9 parse* (lex9 ts9a))

(send gg9 parse (lex9 ts9b))
(send gg9 parse* (lex9 ts9b))

;; --------------------

;; skip example 10, no real point

;; --------------------

(eprintf "\nExample 11:\n")
;; Another example of implicit-end
(define-grammar g11
  [E [(lparen T rparen) #:auto]]
  [T [(atom) #:auto]
     [(T op T) #:auto]
     [(E) #:auto]])
(define gg11 (lr-parser #:grammar g11 #:start E #:implicit-end))

(define lex11 lex5)
(define ts11a "(12 + 72 + 300)")

(send gg11 parse (lex11 ts11a))
(send gg11 parse* (lex11 ts11a))

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

(send gg12 parse (lex11 ts11a))
(send gg12 parse* (lex11 ts11a))

;; ========================================
;; Parameters

(eprintf "\nExample P 1\n")
(define-grammar p1
  [S [(P A) $2]]
  [P [() (list 1 2)]]
  [A #:context [xy]
     [(a) (list xy $1)]])

(define gp1 (lr-parser #:grammar p1 #:start S))

(define lexp1 (words-lexer '(a)))
(define tss1a "a")

(send gp1 parse (lexp1 tss1a))
(send gp1 parse* (lexp1 tss1a))


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

(define lexf1 (words-lexer '(x)))
(define tsf1a "xxxx")

;;(send fp1 parse (lexf1 tsf1a))
(send fp1 parse* (lexf1 tsf1a))
