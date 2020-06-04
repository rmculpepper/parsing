#lang racket/base
(require racket/match
         racket/class
         racket/struct
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
;; Tokenizers

;; Tokenizer = (tokenizer TokenReaderDispatch (-> Void))
;; TokenReaderDispatch = (Symbol List -> Token)
;; - get-token first calls commit-last, then peeks/reads the next token
;; - commit-last commits the previous peek
(struct tokenizer (get-token commit-last))

;; Peek vs Read: a tokenizer's get-token function can either peek or read from
;; its input to get the next token. If a tokenizer always peeks, then a parser
;; can consume exactly the bytes corresponding to the start nonterminal, without
;; consuming the trailing lookahead token, for example.


;; ============================================================
;; Disambiguation filters

(struct filter:reject () #:prefab)

;; ============================================================
;; Collecting values

(struct action:collect (value) #:prefab)

(struct collect-box (vs) #:mutable
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (self) 'collect-box)
   (lambda (self)
     (match self
       [(collect-box (? list? vs)) (list vs)]
       [(collect-box _) (unquoted-printing-string "...")]))))
(define (collect-box-contents cb)
  (match cb
    [(collect-box (? list? vs))
     vs]
    [(collect-box (? hash? h))
     (error 'collect-box-contents "contents are not ready")]))
(define (collect-box-finish! cb)
  (set-collect-box-vs! cb (hash-keys (collect-box-vs cb))))

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
