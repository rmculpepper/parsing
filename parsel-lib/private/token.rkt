;; Copyright 2019-2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require (for-syntax racket/base syntax/transformer)
         racket/match)
(provide (all-defined-out))

;; A DatumTerminal is one of
;; - Character
;; - ExactNonnegativeInteger
;; (may be extended with other quotable datatypes)

(define (datum-terminal? v)
  (or (char? v) (exact-nonnegative-integer? v)))

;; A TokenName is
;; - Symbol         -- terminal or non-terminal
;; - DatumTerminal  -- terminal only

;; A Token is one of
;; - TokenName
;; - (tke TokenName)
;; - (tkv TokenName Any)
;; - (tke/src TokenName StartLoc EndLoc)
;; - (tkv/src TokenName Payload StartLoc EndLoc)
(struct tke (t) #:reflection-name 'token #:transparent)
(struct tkv (t v) #:reflection-name 'token #:transparent)
(struct tke/src tke (s e) #:reflection-name 'token #:transparent)
(struct tkv/src tkv (s e) #:reflection-name 'token #:transparent)

(define (token? v)
  (or (symbol? v) (tke? v) (tkv? v) (datum-terminal? v)))

(define (token-with-value? v)
  (tkv? v))
(define (token-without-value? v)
  (or (symbol? v) (tke? v) (datum-terminal? v)))

(define (token-with-source? v)
  (or (tke/src? v) (tkv/src? v)))

(define (ok-empty-token-name? v)
  (or (symbol? v) (datum-terminal? v)))

(define (make-token0 n)
  (cond [(ok-empty-token-name? n) (tke n)]
        [else (raise-argument-error 'token "ok-empty-token-name?" n)]))

(define make-token
  (case-lambda
    [(t) (token0 t)]
    [(t v)
     (unless (symbol? t)
       (raise-argument-error 'token "symbol?" t))
     (tkv t v)]
    [(t s e)
     (unless (ok-empty-token-name? t)
       (raise-argument-error 'token "ok-empty-token-name?" t))
     (cond [(and s e) (tke/src t s e)]
           [(or s e)
            (error 'token
                   (string-append
                    "start and end locations must be both present or both false"
                    "\n  name: ~e\n  start: ~e\n  end: ~e")
                   t s e)]
           [else t])]
    [(t v s e)
     (unless (symbol? t)
       (raise-argument-error 'token "symbol?" t))
     (cond [(and s e) (tkv/src t v s e)]
           [(or s e)
            (error 'token
                   (string-append
                    "start and end locations must be both present or both false"
                    "\n  name: ~e\n  value: ~e\n  start: ~e\n  end: ~e")
                   t v s e)]
           [else (tkv t v)])]))

;; token-name : Token -> TokenName
(define (token-name x)
  (match x
    [(? symbol? x) x]
    [(tke t) t]
    [(tkv t _) t]
    [(? datum-terminal? x) x]))

;; token-value : Token -> Any
(define (token-value t)
  (cond [(tkv? t) (tkv-v t)]
        [(not (token? t))
         (raise-argument-error 'token-value "token?" t)]
        [else (error 'token-value "token has no value\n  token: ~e" t)]))

;; token-sources : Token -> (values StartLoc EndLoc) or (values #f #f)
(define (token-sources t)
  (match t
    [(tke/src _ s e) (values s e)]
    [(tkv/src _ _ s e) (values s e)]
    [_ (if (token? t)
           (values #f #f)
           (raise-argument-error 'token-sources "token?" t))]))

(define-match-expander token0
  (lambda (stx)
    (syntax-case stx ()
      [(_ t) #'(? token-without-value? (app token-name t))]))
  (make-variable-like-transformer #'make-token0))

(define-match-expander token
  (lambda (stx)
    (syntax-case stx ()
      ;; Note: the 1,2-arg patterns match anything the 3,4-arg patterns match.
      ;; Use, eg, (token t v #f #f) 
      [(_ t) #'(? token-without-value? (app token-name t))]
      [(_ t v) #'(tkv t v)]
      [(_ t s e) #'(? token-without-value? (app token-name t) (app token-sources s e))]
      [(_ t v s e) #'(? tkv? (tkv t v) (app token-sources s e))]))
  (make-variable-like-transformer #'make-token))

#;
(define-match-expander token/sources
  (lambda (stx)
    (syntax-case stx ()
      ;; Note: only matches if sources are present.
      [(_ t s e) #'(tke/src t s e)]
      [(_ t v s e) #'(tkv/src t v s e)])))

;; tokens-start+end : (Listof Token) -> (values StartLoc EndLoc) or (values #f #f)
(define (tokens-start+end ts)
  (let loop ([ts ts] [s #f] [e #f])
    (match ts
      ['() (values s e)]
      [(cons (tke/src _ s0 e0) ts)
       (loop ts (or s s0) (or e0 e))]
      [(cons (tkv/src _ _ s0 e0) ts)
       (loop ts (or s s0) (or e0 e))])))
