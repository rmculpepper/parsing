#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     syntax/parse
                     "grammar-rep.rkt"
                     "../util/datum-to-expr.rkt"
                     "ll1-analysis.rkt")
         racket/class
         racket/lazy-require
         "common.rkt"
         "syntax.rkt")
(provide (all-defined-out))

(lazy-require
 ["ll1-analysis.rkt" (make-LL1)])

(begin-for-syntax
  (define (make-parser-expr g)
    (define pg (make-LL1 g))
    (define table (send pg get-table))
    (define vals-expr (datum->expression (send pg get-vals) (lambda (v) (if (syntax? v) v #f))))
    #`(make-ll1-parser (quote #,table) #,vals-expr (quote #,g))))

(define-syntax (ll1-parser stx)
  (syntax-parse stx
    [(_ #:start start def ...)
     (make-parser-expr (parse-grammar #'start #'(def ...) #:context stx))]
    [(_ #:grammar (~var g (static grammar? "grammar")))
     (make-parser-expr (attribute g.value))]))

(define ll1-parser%
  (class object%
    (init-field table vals g)
    (super-new)
    (define/public (parse get-token)
      (ll1-parse (grammar-start g) table vals get-token))
    (define/public (print)
      (define rt (make-LL1 g))
      (send rt print))
    ))

(define (make-ll1-parser table vals g)
  (new ll1-parser% (table table) (vals vals) (g g)))

;; ============================================================

(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar-rep.rkt")

(define (ll1-parse start table vals tz)
  (define (get-token peek? tr stack)
    (cond [(symbol? tr)
           (tz peek? tr null)]
          [(pair? tr)
           (tz peek? (car tr) (eval-user-expr (cdr tr) stack))]
          [else (error 'll1-parse "bad tr: ~e" tr)]))
  (define (eval-user-expr ue stack)
    (apply (get-val (expr:user-fun ue))
           (get-token-args (expr:user-args ue) stack)))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        [(list datum) datum]
        [(? exact-nonnegative-integer? index)
         (list-ref stack index)])))
  (define (get-val k) (vector-ref vals k))

  (define (loop-nt nt)
    (match (hash-ref table nt)
      [(list* tr ctxn dispatch)
       (define next-tok (get-token #t tr null))
       (cond [(hash-ref dispatch (token-name next-tok) #f)
              => (lambda (ps) (loop-prod (car ps) ctxn))]
             [else (error 'll1-parse "NT = ~v, next = ~v" nt next-tok)])]))

  (define (loop-elem e lstack)
    (match e
      [(ntelem nt)
       (cons (token nt (loop-nt nt)) lstack)]
      [(telem t tr)
       (loop-token t (get-token #f tr lstack) lstack)]
      [(pure-elem t ue)
       (loop-token t (eval-user-expr ue lstack) lstack)]))

  (define (loop-token t next-tok lstack)
    (if (eqv? t (token-name next-tok))
        (cons next-tok lstack)
        (error 'll1-parse "expected ~v, next = ~v" t next-tok)))

  (define (loop-prod p ctxn)
    (match-define (prod nt index item action) p)
    (unless (zero? ctxn) (error 'll1-parse "parameters not implemented"))
    (apply-action action
                  (for/fold ([lstack null]) ([e (in-vector item)])
                    (loop-elem e lstack))))

  (define (apply-action action lstack)
    (apply (get-val action) (reverse lstack)))

  (loop-nt start))

;; FIXME: don't need to store (token NT/T Value) on stack, just Value,
;; *except* for detecting tokens without payloads in action routines.

(define (apply->token f args)
  (define v (apply f args))
  (list (if (ok-terminal? v) v 'bad-token-name)))
