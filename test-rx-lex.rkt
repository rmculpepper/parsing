#lang racket/base
(require racket/match
         racket/pretty
         "private/common.rkt"
         "private/lex-rx.rkt")
(provide (all-defined-out))

(define tr
  (regexps-token-reader
   #rx#"[a-zA-Z][a-zA-Z0-9]*"
   (lambda (lexeme start end) (token 'identifier lexeme))
   #rx"[0-9]+"
   (lambda (lexeme start end) (token 'number (string->number lexeme)))
   #rx"[ \t\n\r]+"
   (lambda _ '#f)
   #rx"\"(?:[^\"]|[\\\\][\"])*\""
   (lambda (lexeme start end)
     (token 'string (regexp-replace* #rx"[\\\\][\"]"
                                     (substring lexeme 1 (sub1 (string-length lexeme)))
                                     "\"")))
   #rx"[#]"
   (lambda (lexeme start end) 'char-next)))

(define (tokenizer-read-all tz)
  (match-define (tokenizer get-token commit-last) tz)
  (let loop ([tr 'default])
    (define tok (get-token tr null))
    (cons tok
          (case (token-name tok)
            [(EOF) null]
            [(char-next) (loop 'char)]
            [else (loop 'default)]))))

(define s #<<EOF
abc 23 0 i18n "hello world!"
def #A #.
EOF
)

(define (open-s [count-lines? #t])
  (define in (open-input-string s))
  (when count-lines? (port-count-lines! in))
  in)

(define (make-s-tz [count-lines? #t])
  (make-tokenizer (open-s count-lines?)
                  tr
                  (hasheq 'char (char-token-reader '(#\.)))))

(pretty-print
 (tokenizer-read-all (make-s-tz)))
