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
         token-end)

;; A TokenValue is one of
;; - safe-terminal
;; - (token Terminal (U Any no-value) StartLoc/#f EndLoc/#f)
;; {Start,End}Loc = any non-false value, eg Nat (don't have to be the same types!)
(struct tk (t v start end) #:prefab #:reflection-name 'token)

(define no-value (string->uninterned-symbol "∅"))

(define (safe-terminal? v)
  (and (ok-terminal? v) (not (boolean? v))))

(define token
  (case-lambda
    [(t) (if (safe-terminal? t) t (tk t no-value #f #f))]
    [(t v) (tk t v #f #f)]
    [(t v s e) (tk t v s e)]))

(define token/no-value
  (case-lambda
    [(t) (if (safe-terminal? t) t (tk t no-value #f #f))]
    [(t s e) (tk t no-value s e)]))

(define (token? v)
  (or (tk? v) (safe-terminal? v)))

(define (token-name x)
  (match x
    [(tk t _ _ _) t]
    [(? safe-terminal?) x]))

(define (token-value x)
  (define (bad) (error 'token-value "token contains no value\n  token: ~e" x))
  (match x
    [(tk _ v _ _) (if (eq? v no-value) (bad) v)]
    [(? safe-terminal?) (bad)]
    [_ (raise-argument-error 'token-value "token?" 0 x)]))

(define (token-value* x [default #f])
  (match x
    [(tk _ v _ _) (if (eq? v no-value) default v)]
    [(? safe-terminal?) default]))

(define (token-with-value? x)
  (match x
    [(tk _ v _ _) (not (eq? v no-value))]
    [_ #f]))

(define (token-no-value? x)
  (and (token? x) (not (token-with-value? x))))

(define (token-start x)
  (match x [(tk _ _ s e) s] [_ #f]))
(define (token-end x)
  (match x [(tk _ _ s e) e] [_ #f]))

(define-match-expander -token
  (lambda (stx)
    (syntax-case stx ()
      [(_ t) #'(? token? (app token-name t))]
      [(_ t v) #'(? token-with-value? (app token-name t) (app token-value v))]
      [(_ t v s e) #'(? token-with-value? (app token-name t) (app token-value v)
                        (app token-start s) (app token-end e))]))
  (make-variable-like-transformer #'token))

(define-match-expander -token/no-value
  (lambda (stx)
    (syntax-case stx ()
      [(_ t) #'(? token? (app token-name t))]
      [(_ t s e) #'(? token/no-value? (app token-name t)
                      (app token-start s) (app token-end e))]))
  (make-variable-like-transformer #'token/no-value))
