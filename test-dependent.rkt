#lang racket/base
(require racket/class
         racket/match
         racket/pretty
         "main.rkt"
         "private/lex-rx.rkt"
         (submod "test.rkt" util))
(provide (all-defined-out)
         (all-from-out "main.rkt"))

(pretty-print-columns 200)

(define PRINT? #f)

;; ============================================================

(eprintf "\nExample d1:\n")
(define-grammar d1
  [Message [([msg1 #:read MsgByte] [len int4 #:read Int4] [data data #:read (read-data len)])
            #:> (list 1 len data)]
           [([msg2 #:read MsgByte] [d data #:read (read-data '8)])
            #:> (list 2 d)]])
(define dg1 (lr-parser #:grammar d1 #:start Message))
(when PRINT? (send dg1 print))

(define sd1a '((msg1) (int4 4)))
(define sd1b '((msg2)))

(define (d1-tokenizer vs)
  (define toks (make-toks vs))
  (define (default-get-token)
    (if (pair? toks) (begin0 (car toks) (set! toks (cdr toks))) (token 'EOF)))
  (tokenizer (lambda (tf args)
               (case tf
                 [(read-data) (token 'data (car args))]
                 [else (default-get-token)]))
             void))

(send dg1 parse (d1-tokenizer sd1a))
(send dg1 parse* (d1-tokenizer sd1a))

(send dg1 parse (d1-tokenizer sd1b))
(send dg1 parse* (d1-tokenizer sd1b))

;; ----------------------------------------

(eprintf "\nExample d2:\n")
(define-grammar d2
  [Phrase [([p Phrase] #\space [w Word]) #:> (append p (list w))]
          [([w Word]) #:> (list w)]]
  [Word [([c letter]) #:> (list c)]
        [([c letter] [w Word]) #:> (cons c w)]])
(define dg2 (lr-parser #:grammar d2 #:start Phrase))
(when PRINT? (send dg2 print))

(define (d2-tokenizer str)
  (define in (open-input-string str))
  (make-tokenizer in (char-token-reader '(#\space) #:other-token-name 'letter)))

(define sd2a "hello world how are you today")
(send dg2 parse (d2-tokenizer sd2a))
(send dg2 parse* (d2-tokenizer sd2a))

;; ----------------------------------------

(eprintf "\nExample d3:\n")
(define-grammar d3
  [Settings [([ss Settings] [#\; #:read char] [s Setting]) #:> (append ss (list s))]
            [([s Setting]) #:> (list s)]]
  [Setting [([c letter #:read char] [#\= #:read char] [w word]) #:> (list c w)]])
(define dg3 (lr-parser #:grammar d3 #:start Settings))
(when PRINT? (send dg3 print))

(define (d3-tokenizer str)
  (define in (open-input-string str))
  (define char-tr (char-token-reader '(#\; #\=) #:other-token-name 'letter))
  (define string-tr (make-token-reader #rx"[^;]*" (lambda (lexeme s e) (token 'word lexeme))))
  (make-tokenizer in string-tr (hasheq 'char char-tr)))

(define sd3a "h=hello;w=world;m=;g=how are you today")
(send dg3 parse (d3-tokenizer sd3a))
(send dg3 parse* (d3-tokenizer sd3a))

;; ?? Maybe allow configurable default TokenReaders,
;; eg #:token-kind ([(#\space) char]) or #:token-kind ([char char])

;; ----

(eprintf "\nExample d3 (LL):\n")
;; Modified to be LL(1) friendly
(define-grammar d3*
  [Settings [([s Setting] [ss MoreSettings]) #:> (cons s ss)]]
  [MoreSettings [([#\; #:read char] [ss Settings]) #:> ss]
                [() #:> null]]
  [Setting [([c letter #:read char] [#\= #:read char] [w word]) #:> (list c w)]])
(define l3 (ll1-parser #:grammar d3* #:start Settings))
(when PRINT? (send l3 print))

(send l3 parse (d3-tokenizer sd3a))

;; ----------------------------------------

(eprintf "\nExample d4:\n")
(define-grammar d4
  [S [([c letter #:read char] [e E] [65 #:top]) #:> (list "got A" c e)]
     [([c letter #:read char] [e E] [66 #:top]) #:> (list "got B" c e)]]
  [E #:context [p]
     [() (char->integer p)]])
(define gd4 (lr-parser #:grammar d4 #:start S))
(when PRINT? (send gd4 print))

(send gd4 parse (d3-tokenizer "A"))
(send gd4 parse* (d3-tokenizer "A"))

(send gd4 parse (d3-tokenizer "B"))
(send gd4 parse* (d3-tokenizer "B"))

;; ----------------------------------------

(eprintf "\nExample d5:\n")
;; Example of test-then-top pattern.
(define-grammar d5
  [S [([m byte] Z [#t #:top]) #:auto]
     [([n byte] Z [#f #:top] [v byte]) #:auto]]
  [Z #:context [x]
     [() (zero? x)]])
(define dg5 (lr-parser #:grammar d5 #:start S))
(when PRINT? (send dg5 print))

(define (d5-tokenizer bstr)
  (define in (open-input-bytes bstr))
  (make-tokenizer in (byte-token-reader '() #:other-token-name 'byte)))

(define sd5a (bytes 1 42))
(define sd5b (bytes 0))

(send dg5 parse (d5-tokenizer sd5a))
(send dg5 parse* (d5-tokenizer sd5a))

(send dg5 parse (d5-tokenizer sd5b))
(send dg5 parse* (d5-tokenizer sd5b))
