#lang racket/base
(require racket/class
         racket/match
         racket/pretty
         "dependent.rkt")
(provide (all-defined-out)
         (all-from-out "dependent.rkt"))

(pretty-print-columns 200)

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

;; ----------------------------------------

(eprintf "\nExample d1:\n")
(define-grammar d1
  #:start Message
  [Message [([msg1 #:read MsgByte] [len int4 #:read Int4] [data data #:read (read-data len)])
            #:> (list 1 len data)]
           [([msg2 #:read MsgByte] [d data #:read (read-data '8)])
            #:> (list 2 d)]])
(define dg1 (lr-parser #:grammar d1))
(send dg1 print)

(define sd1a '((msg1) (int4 4)))
(define sd1b '((msg2)))

(define (d1-tokenizer toks)
  (peeking-tokenizer
   (lambda (peek? kind args)
     (case kind
       [(read-data) (tok 'data (car args))]
       [else (if (pair? toks) (begin0 (apply tok (car toks)) (set! toks (cdr toks))) EOF-tok)]))))

(send dg1 parse (d1-tokenizer sd1a))
(send dg1 parse (d1-tokenizer sd1b))

;; ----------------------------------------

(eprintf "\nExample d2:\n")
(define-grammar d2
  #:start Phrase
  [Phrase [([p Phrase] #\space [w Word]) #:> (append p (list w))]
          [([w Word]) #:> (list w)]]
  [Word [([c letter]) #:> (list c)]
        [([c letter] [w Word]) #:> (cons c w)]])
(define dg2 (lr-parser #:grammar d2))
(send dg2 print)

(define (d2-tokenizer str)
  (define in (open-input-string str))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (get-char-token in #:token-name 'letter #:special '(#\space)))))

(define sd2a "hello world how are you today")
(send dg2 parse (d2-tokenizer sd2a))

;; ----------------------------------------

(eprintf "\nExample d3:\n")
(define-grammar d3
  #:start Settings
  [Settings [([ss Settings] [#\; #:read char] [s Setting]) #:> (append ss (list s))]
            [([s Setting]) #:> (list s)]]
  [Setting [([c letter #:read char] [#\= #:read char] [w word]) #:> (list c w)]])
(define dg3 (lr-parser #:grammar d3))
(send dg3 print)

(define (d3-tokenizer str)
  (define in (open-input-string str))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (case kind
       [(char) (get-char-token in #:token-name 'letter #:special '(#\; #\=))]
       [else (get-string-token in #:token-name 'word #:delimiters '(#\;))]))))

(define sd3a "h=hello;w=world;m=;g=how are you today")
(send dg3 parse (d3-tokenizer sd3a))

;; ?? Maybe allow configurable default TokenReaders,
;; eg #:token-kind ([(#\space) char]) or #:token-kind ([char char])

;; ----------------------------------------

(eprintf "\nExample d4:\n")
(define-grammar d4
  #:start S
  [S [([m byte] [#t #:apply (zero? m)]) #:> 'none]
     [([n byte] [#f #:apply (zero? n)] [v byte]) #:> v]])
(define dg4 (lr-parser #:grammar d4))
(send dg4 print)

(define (d4-tokenizer bstr)
  (define in (open-input-bytes bstr))
  (peeking-tokenizer
   (lambda (peek? kind args)
     (get-byte-token in))))

(define sd4a (bytes 1 42))
(define sd4b (bytes 0))
(send dg4 parse (d4-tokenizer sd4a))
(send dg4 parse (d4-tokenizer sd4b))
