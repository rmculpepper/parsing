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

(define ((peeking-lexer lx) src)
  (define in (source->input-port 'peeking-lexer src))
  (if (string-port? in)
      (rewinding-tokenizer lx in)
      (peeking-tokenizer lx in)))

(define (source->input-port who src)
  (cond [(input-port? src) src]
        [(string? src) (let ([in (open-input-string src)]) (port-count-lines! in) in)]
        [(bytes? src) (let ([in (open-input-bytes src)]) (port-count-lines! in) in)]))

(define (peeking-tokenizer lx in)
  (local-require (only-in racket/port peeking-input-port))
  (define peek-in (peeking-input-port in))
  (file-stream-buffer-mode peek-in 'none) ;; ?? might change in's mode ?!
  (port-count-lines! peek-in)
  (call-with-values (lambda () (port-next-location in))
                    (lambda vs (apply set-port-next-location! peek-in vs)))
  (match-define (tokenizer tz-get-token tz-commit-last) (lx peek-in))
  (define last-peek-amt 0)
  (define (get-token tf args)
    (commit-last)
    (tz-get-token tf args))
  (define (commit-last)
    (define saved-fpos (file-position peek-in))
    (tz-commit-last)
    (define diff (- (file-position peek-in) saved-fpos))
    (void (read-bytes diff in)))
  (tokenizer get-token commit-last))

(define (rewinding-tokenizer lx in)
  (match-define (tokenizer tz-get-token tz-commit-last) (lx in))
  (define last-state #f) ;; (list fpos line col pos) or #f
  (define (set-state state)
    (when state
      (file-position in (car state))
      (apply set-port-next-location! in (cdr state))))
  (define (get-state)
    (cons (file-position in)
          (call-with-values (lambda () (port-next-location in)) list)))
  (define (get-token tf args)
    (define saved-state (get-state))
    (set-state last-state)
    (begin0 (tz-get-token tf args)
      (set! last-state (get-state))
      (set-state saved-state)))
  (define (commit-last)
    (set-state last-state)
    (tz-commit-last)
    (set! last-state #f))
  (tokenizer get-token commit-last))


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
