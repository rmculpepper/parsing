#lang racket/base
(require (for-syntax racket/base syntax/transformer)
         racket/match
         (submod "grammar-rep.rkt" common))
(provide (rename-out
          [-token token]
          [-token/no-value token/no-value])
         token?
         token-with-value?
         token-name
         token-value
         token-value*
         token-start
         token-end
         token-add-locations)

;; A TokenValue is one of
;; - safe-terminal
;; - (token:e Terminal StartLoc/#f EndLoc/#f)
;; - (token:v Terminal Any StartLoc/#f EndLoc/#f)
;; {Start,End}Loc = any non-false value, eg Nat (don't have to be the same types!)
(struct tke (t s e) #:prefab #:reflection-name 'etoken)
(struct tkv (t v s e) #:prefab #:reflection-name 'vtoken)

(define (safe-terminal? v)
  (and (ok-terminal? v) (not (boolean? v))))

(define token
  (case-lambda
    [(t) (token/no-value t)]
    [(t v) (tkv t v #f #f)]
    [(t v s e) (tkv t v s e)]))

(define token/no-value
  (case-lambda
    [(t) (if (safe-terminal? t) t (tke t #f #f))]
    [(t s e) (if (or s e) (tke t s e) (token/no-value t))]))

(define (token? v)
  (or (tke? v) (tkv? v) (safe-terminal? v)))

(define (token-name x)
  (match x
    [(tke t _ _) t]
    [(tkv t _ _ _) t]
    [(? safe-terminal?) x]))

(define (token-value x)
  (define (bad) (error 'token-value "token contains no value\n  token: ~e" x))
  (match x
    [(tkv _ v _ _) v]
    [(? tke?) (bad)]
    [(? safe-terminal?) (bad)]
    [_ (raise-argument-error 'token-value "token?" 0 x)]))

(define (token-value* x [default #f])
  (match x
    [(tkv _ v _ _) v]
    [(? tke?) default]
    [(? safe-terminal?) default]))

(define (token-with-value? x) (tkv? x))

(define (token-no-value? x)
  (and (token? x) (not (token-with-value? x))))

(define (token-start x)
  (match x
    [(tke _ s e) s]
    [(tkv _ _ s e) s]
    [_ #f]))
(define (token-end x)
  (match x
    [(tke _ s e) e]
    [(tkv _ _ s e) e]
    [_ #f]))

(define (token-add-locations x s e)
  (match x
    [(tke t s0 e0) (if (or s0 e0) x (tke t s e))]
    [(tkv t v s0 e0) (if (or s0 e0) x (tkv t v s e))]
    [(? safe-terminal? t) (if (or s e) (tke t s e) t)]))

(define-match-expander -token
  (lambda (stx)
    (syntax-case stx ()
      [(_ t) #'(? token? (app token-name t))]
      [(_ t v) #'(tkv t v _ _)]
      [(_ t v s e) #'(tkv t v s e)]))
  (make-variable-like-transformer #'token))

(define-match-expander -token/no-value
  (lambda (stx)
    (syntax-case stx ()
      [(_ t) #'(? token? (app token-name t))]
      [(_ t s e) #'(? token/no-value? (app token-name t)
                      (app token-start s) (app token-end e))]))
  (make-variable-like-transformer #'token/no-value))
