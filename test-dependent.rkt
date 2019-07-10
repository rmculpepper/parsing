#lang racket/base
(require racket/class
         racket/match
         racket/pretty
         "dependent.rkt")
(provide (all-defined-out)
         (all-from-out "dependent.rkt"))

(pretty-print-columns 100)

;; ============================================================

(define grammar% (LR-mixin grammar-base%))

;; ============================================================

#|
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
|#

;; ============================================================

(define ((mktokenizer ktoks) peek? kind args)
  (match ktoks
    [(cons (cons tk tok) ktoks*)
     (unless (equal? tk kind)
       (error 'tokenizer "parser requested kind ~e, but token has kind ~e" kind tk))
     (set! ktoks ktoks*)
     tok]
    [_ EOF-tok]))

(define ((mk-msg-tz* toks) peek? kind args)
  (case kind
    [(read-data) (list 'data (car args))]
    [else (if (pair? toks) (begin0 (car toks) (set! toks (cdr toks))) EOF-tok)]))

(define (mk-msg-tz toks) (peeking-tokenizer (mk-msg-tz* toks)))

;; ----------------------------------------

(eprintf "\nExample d1:\n")
(define d1
  (DGrammar
   #:start Message
   [Message ([msg1 #:kind MsgByte] [len int4 #:kind Int4] [data data #:call (read-data len)])
            ([msg2 #:kind MsgByte] [data #:call (read-data '8)])]))
(define dg1 (new grammar% (g d1)))
(send dg1 print)

;;(define sd1a '((MsgByte msg1) (Int4 int4) (read-data data)))
;;(define sd1b '((MsgByte msg2) (read-data data)))

(define sd1a '((msg1) (int4 4)))
(define sd1b '((msg2)))

(send dg1 lr0-parse (mk-msg-tz sd1a))
(send dg1 lr0-parse (mk-msg-tz sd1b))

;; ----------------------------------------

(eprintf "\nExample d2:\n")
(define d2
  (DGrammar
   #:start Phrase
   [Phrase (Phrase #\space Word) (Word)]
   [Word (letter) (letter Word)]))
(define dg2 (new grammar% (g d2)))
(send dg2 print)

(define (d2-tokenizer str)
  (define in (open-input-string str))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (let ([c (read-char in)])
       (cond [(eof-object? c) EOF-tok]
             [(eqv? c #\space) (list #\space)]
             [else (list 'letter c)])))))

(define (reduce2 v)
  (match v
    [(list 'Phrase 0 p sp w) (append (reduce2 p) (list (reduce2 w)))]
    [(list 'Phrase 1 w) (list (reduce2 w))]
    [(list 'Word 0 c) (list (reduce2 c))]
    [(list 'Word 1 c w) (cons (reduce2 c) (reduce2 w))]
    [(list 'letter c) c]))

(define sd2a "hello world how are you today")
(reduce2 (send dg2 lr0-parse (d2-tokenizer sd2a)))

;; ----------------------------------------

(eprintf "\nExample d3:\n")
(define d3
  (DGrammar
   #:start Settings
   [Settings (Settings [#\; #:kind char] Setting) (Setting)]
   [Setting ([c letter #:kind char] [#\= #:kind char] word)]))
(define dg3 (new grammar% (g d3)))
(send dg3 print)

(define (reduce3 v)
  (let loop ([v v])
    (match v
      [(list 'Settings 0 ss _ s) (append (loop ss) (list (loop s)))]
      [(list 'Settings 1 s) (list (loop s))]
      [(list 'Setting 0 c _ w) (list (loop c) (loop w))]
      [(list 'letter c) c]
      [(list 'word w) w])))

(define (d3-tokenizer str)
  (define in (open-input-string str))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (case kind
       [(char)
        (let ([c (read-char in)])
          (cond [(eof-object? c) EOF-tok]
                [(eqv? c #\;) (list #\;)]
                [(eqv? c #\=) (list #\=)]
                [else (list 'letter c)]))]
       [else
        (let loop ([acc null])
          (let ([c (peek-char in)])
            (cond [(eof-object? c)
                   (if (null? acc) EOF-tok (list 'word (apply string (reverse acc))))]
                  [(eqv? c #\;)
                   (list 'word (apply string (reverse acc)))]
                  [else (loop (cons (read-char in) acc))])))]))))

(define sd3a "h=hello;w=world;m=;g=how are you today")
(reduce3 (send dg3 lr0-parse (d3-tokenizer sd3a)))



;; IDEA: make char literal have TokenKind 'char by default
;; IDEA: have integer literal have TokenKind 'integer by default
;; No, bad idea. But maybe allow configurable defaults?
;; eg #:token-kind ([(#\space) char]) or #:token-kind ([char char])
