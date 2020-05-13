#lang racket/base
(require racket/match
         racket/generic
         (submod "grammar-rep.rkt" common)
         "token.rkt")
(provide (all-defined-out)
         (all-from-out "token.rkt")
         (all-from-out (submod "grammar-rep.rkt" common)))

;; Common runtime support code

;; ============================================================

;; make-nt-token : NT Any (Listof Token) -> Token
(define (make-nt-token nt value args)
  (token nt value
         ;; start = *first* token-start from args
         (ormap token-start args)
         ;; end = *last* token-end from args
         (for/fold ([end #f]) ([arg (in-list args)]) (or (token-end arg) end))))

(define (get-token-value who tok)
  (if (token-with-value? tok)
      (token-value tok)
      (error who "token has no payload\n  token: ~e" tok)))

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
  (cond [(eof-object? next) (token 'EOF)]
        [(memv next special) (begin (read-char in) (token next next))]
        [else (begin (read-char in) (token tname next))]))

(define (get-byte-token in #:token-name [tname 'byte] #:special [special null])
  (define next (peek-byte in))
  (cond [(eof-object? next) (token 'EOF)]
        [(memv next special) (begin (read-byte in) (token next next))]
        [else (begin (read-byte in) (token tname next))]))

(define (get-string-token in #:token-name [tname 'string] #:delimiters [delims null])
  (define next (peek-char in))
  (cond [(eof-object? next) (token 'EOF)]
        [else
         (define out (open-output-string))
         (let loop ()
           (define next (peek-char in))
           (cond [(or (eof-object? next) (memv next delims))
                  (token tname (get-output-string out))]
                 [else (begin (read-char in) (write-char next out) (loop))]))]))

;; ============================================================
;; Disambiguation filters

(struct filter:reject () #:prefab)

;; ============================================================
;; Error reporting

(struct exn:fail:parse exn:fail (context)
  #:property prop:exn:srclocs
  (lambda (self) (context->srclocs (exn:fail:parse-context self))))

(define (parse-error who context)
  (let/ec here
    (raise (exn:fail:parse (format "~s: parse error" who)
                           (continuation-marks here)
                           context))))

(define-generics context
  (context->stack context) ;; Context -> (Listof (U Token PrettyState))
  (context->stacks context) ;; Context -> (Listof (Listof (U Token PrettyState)))
  (context->expected-terminals context) ;; Context -> (U #f (Listof Terminal))
  (context->srclocs context) ;; Context -> (Listof srcloc)
  #:fallbacks
  [(define (context->srclocs self) null)])
