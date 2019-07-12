#lang racket/base
(require racket/match
         "grammar-rep.rkt")
(provide (all-defined-out))

;; ============================================================

;; A TokenValue is one of
;; - Terminal                       -- token without payload
;; - (cons Terminal TokenPayload)   -- token with payload

(define tok (case-lambda [(t) t] [(t v) (cons t v)]))

(define (tok-t tok)
  (match tok [(cons t _) t] [(? ok-terminal? t) t]))

(define (tok-v tok) ;; returns #f is no payload
  (match tok [(cons _ v) v] [(? ok-terminal? t) #f]))

(define (tok-has-v? tok) (pair? tok))

(define (get-token-value who tok)
  (match tok
    [(cons _ v) v]
    [(? ok-terminal? t) (error who "token has no payload\n  token: ~e" t)]))

(define EOF (string->uninterned-symbol "EOF"))
(define EOF-tok EOF)

;; ============================================================

;; A Tokenizer is (Boolean Symbol (Listof Arg) -> Token).
;; The tokenizer should be aware of peek vs read, so that for example
;; on input ports it can implement token-peeking by port-peeking.

;; A SimpleTokenizer is (-> Token).

(define (peeking-tokenizer tz)
  (define peeked #f)
  (define (tokenize peek? kind args)
    (cond [peeked (begin0 peeked (unless peek? (set! peeked #f)))]
          [peek? (let ([v (tz #f kind args)]) (set! peeked v) v)]
          [else (tz #f kind args)]))
  tokenize)

(define (dispatch-tokenizer h)
  (define (tokenize peek? kind args)
    (cond [(hash-ref h kind #f)
           => (lambda (v)
                (cond [(procedure? v)
                       (if (null? args) (v) (dispatch-arity-error kind 0 args))]
                      [(= (car v) (length args)) (apply (cdr v) args)]
                      [else (dispatch-arity-error kind (car v) null)]))]
          [else (error 'tokenizer "unknown token kind\n  name: ~e" kind)]))
  tokenize)

(define (dispatch-arity-error kind arity args)
  (cond [(zero? arity)
         (error 'tokenizer
                (string-append "token kind used with arguments"
                               "\n  token kind: ~s"
                               "\n  given: ~e")
                kind args)]
        [else
         (error 'tokenizer
                (string-append "token function arity mismatch"
                               "\n  token function: ~s"
                               "\n  expected: ~s arguments"
                               "\n  given: ~e")
                kind arity args)]))

(define (get-char-token in #:token-name [tname 'char] #:special [special null])
  (define next (peek-char in))
  (cond [(eof-object? next) EOF-tok]
        [(memv next special) (begin (read-char in) (tok next next))]
        [else (begin (read-char in) (tok tname next))]))

(define (get-byte-token in #:token-name [tname 'byte] #:special [special null])
  (define next (peek-byte in))
  (cond [(eof-object? next) EOF-tok]
        [(memv next special) (begin (read-byte in) (tok next next))]
        [else (begin (read-byte in) (tok tname next))]))

(define (get-string-token in #:token-name [tname 'string] #:delimiters [delims null])
  (define next (peek-char in))
  (cond [(eof-object? next) EOF-tok]
        [else
         (define out (open-output-string))
         (let loop ()
           (define next (peek-char in))
           (cond [(or (eof-object? next) (memv next delims))
                  (tok tname (get-output-string out))]
                 [else (begin (read-char in) (write-char next out) (loop))]))]))

;; ============================================================
