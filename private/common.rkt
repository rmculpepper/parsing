#lang racket/base
(require racket/match
         racket/class
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

(struct tokenizer
  (get-token    ;; Symbol (Listof Arg) -> Token
   commit-last  ;; -> Void
   ))
;; - get-token first calls commit-last, then peeks the next token
;; - commit-last commits the previous peek

#;
(define (make-tokenizer read-token in
                        #:can-rewind? [can-rewind? (string-port? in)])
  (if can-rewind?
      (make-rewinding-tokenizer read-token in)
      (make-peeking-tokenizer read-token in)))

#;
(define (make-rewinding-tokenizer read-token in)
  (define last-loc #f)
  (define last-fpos #f)
  (define (get-loc)
    (define-values (line col pos) (port-next-location in))
    (vector line col pos))
  (define (get-token tf args)
    (mark-last #t)
    (set! last-loc (get-loc))
    (set! last-fpos (file-position in))
    (read-token tf args in))
  (define (mark-last mode)
    (case mode
      [(commit)
       (void)]
      [(rollback)
       (when last-fpos
         (file-position last-fpos))
       (when last-loc
         (apply set-port-next-location! in (vector->list last-loc)))]))
  (tokenizer get-token mark-last get-loc))

#;
(define (make-peeking-tokenizer read-token in)
  (define peek-in (peeking-input-port in))
  (file-stream-buffer-mode peek-in 'none) ;; ?? might change in's mode ?!
  (port-count-lines! peek-in)
  (call-with-values (lambda () (port-next-location in))
                    (lambda args) (apply set-port-next-location! peek-in args))
  (define last-read-amt 0)
  (define (get-loc)
    (define-values (line col pos) (port-next-location peek-in))
    (vector line col pos))
  (define (get-token tf args)
    (mark-last #t)
    (define pre-fpos (file-position peek-in))
    (begin0 (read-token tf args peek-in)
      (set! last-read-amt (- (file-position peek-in) pre-fpos))))
  (define (mark-last mode)
    (case mode
      [(commit)
       (when (> last-read-amt 0)
         (void (read-bytes last-read-amt in))
         (set! last-read-amt 0))]
      [(rollback)
       (void)]))
  (tokenizer get-token mark-last get-loc))

;; A Tokenizer is (Boolean Symbol (Listof Arg) -> Token).
;; The tokenizer should be aware of peek vs read, so that for example
;; on input ports it can implement token-peeking by port-peeking.

;; A SimpleTokenizer is (-> Token).

#|
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
|#

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
    (raise (exn:fail:parse (format "~s: parse error~a"
                                   who (context->error-lines context))
                           (continuation-marks here)
                           context))))

(define-generics context
  (context->stack context) ;; Context -> (Listof (U Token PrettyState))
  (context->stacks context) ;; Context -> (Listof (Listof (U Token PrettyState)))
  (context->expected-terminals context) ;; Context -> (U #f (Listof Terminal))
  (context->srclocs context) ;; Context -> (Listof srcloc)
  (context->error-lines context) ;; Context -> String
  #:fallbacks
  [(define (context->srclocs self) null)
   (define (context->error-lines self) "")])
