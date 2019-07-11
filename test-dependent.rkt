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
   [Message ([msg1 #:read MsgByte] [len int4 #:read Int4] [data data #:read (read-data len)])
            ([msg2 #:read MsgByte] [data #:read (read-data '8)])]))
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
     (get-char-token in #:token-name 'letter #:special '(#\space)))))

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
   [Settings (Settings [#\; #:read char] Setting) (Setting)]
   [Setting ([c letter #:read char] [#\= #:read char] word)]))
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
       [(char) (get-char-token in #:token-name 'letter #:special '(#\; #\=))]
       [else (get-string-token in #:token-name 'word #:delimiters '(#\;))]))))

(define sd3a "h=hello;w=world;m=;g=how are you today")
(reduce3 (send dg3 lr0-parse (d3-tokenizer sd3a)))

;; IDEA: make char literal have TokenKind 'char by default
;; IDEA: have integer literal have TokenKind 'integer by default
;; No, bad idea. But maybe allow configurable defaults?
;; eg #:token-kind ([(#\space) char]) or #:token-kind ([char char])

;; ----------------------------------------

(eprintf "\nExample d4:\n")
(define d4
  (DGrammar
   #:start S
   [S ([m byte] [#t #:apply (zero? m)])
      ([n byte] [#f #:apply (zero? n)] [v byte])]))
(define dg4 (new grammar% (g d4)))
(send dg4 print)

(define (d4-tokenizer bstr)
  (define in (open-input-bytes bstr))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (get-byte-token in))))

(define sd4a (bytes 1 42))
(define sd4b (bytes 0))
(send dg4 lr0-parse (d4-tokenizer sd4a))
(send dg4 lr0-parse (d4-tokenizer sd4b))
