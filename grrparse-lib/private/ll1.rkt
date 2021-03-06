#lang racket/base
(require (for-syntax racket/base
                     racket/class
                     (rename-in syntax/parse [attribute $])
                     "grammar-rep.rkt"
                     "ll1-analysis.rkt")
         racket/match
         racket/class
         racket/promise
         "grammar-rep.rkt"
         "common.rkt"
         "syntax.rkt"
         "analysis-runtime.rkt")
(provide (all-defined-out))

(begin-for-syntax
  (define (make-parser-expr g+)
    (define pg (make-LL1 g+))
    (define table (send pg get-table))
    (define vals-expr (send pg get-vals))
    #`(make-ll1-parser (quote #,table)
                       (quote #,(grammar+-start g+))
                       #,vals-expr
                       (quote #,(send pg get-summary-data)))))

(define-syntax (ll1-parser stx)
  (syntax-parse stx
    [(_ spec:grammar+start+end)
     (make-parser-expr ($ spec.ast))]))

(define ll1-parser%
  (class object%
    (init-field table start vals)
    (init summary-data)
    (super-new)

    (define/public (parse tz)
      (ll1-parse start table vals tz))

    (define rt (delay (new LL1-done% (summary-data summary-data))))
    (define/public (print)
      (send (force rt) print))
    ))

(define (make-ll1-parser table start vals summary-data)
  (new ll1-parser% (table table) (start start) (vals vals) (summary-data summary-data)))

;; ============================================================

(define (ll1-parse start table vals tz)
  (match-define (tokenizer tz-get-token tz-commit-last) tz)
  (define peeked-token #f)
  (define (peek-token tr stack)
    (cond [peeked-token peeked-token]
          [else
           (define tok (get-token tr stack))
           (set! peeked-token tok)
           tok]))
  (define (read-token tr stack)
    (begin0 (peek-token tr stack)
      (set! peeked-token #f)))
  (define (get-token tr stack)
    (cond [(symbol? tr)
           (tz-get-token tr null)]
          [(pair? tr)
           (tz-get-token (car tr) (eval-user-expr (cdr tr) stack))]
          [else (error 'll1-parse "bad token reader: ~e" tr)]))
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

  (define (loop-nt nt stack)
    (match (hash-ref table nt)
      [(list* tr ctxn dispatch)
       (define next-tok (peek-token tr null))
       (cond [(hash-ref dispatch (token-name next-tok) #f)
              => (lambda (ps) (loop-prod (car ps) ctxn stack))]
             [else (fail `(nt ,nt ,(hash-keys dispatch)) (cons next-tok stack))])]))

  (define (loop-elem e stack)
    (match e
      [(ntelem nt)
       (cons (loop-nt nt stack) stack)]
      [(telem t tr)
       (define next-tok (read-token tr stack))
       (if (eqv? t (token-name next-tok))
           (cons next-tok stack)
           (fail `(terminal ,t) (cons next-tok stack)))]
      [(top-elem t)
       (if (eqv? t (token-value (car stack)))
           stack
           (fail `(top ,t) stack))]))

  (define (loop-prod p ctxn stack)
    (match-define (prod nt index item action) p)
    (define stack* (for/fold ([stack stack]) ([e (in-vector item)]) (loop-elem e stack)))
    (apply-action nt (get-val action) (vector-length item) ctxn stack*))

  (define (fail how stack) (parse-error 'll1-parser (ll1-context how stack)))

  (loop-nt start null))

(define (apply-action nt action-fun popn peekn stack)
  (define-values (args stack*) (rev-take popn stack null))
  (define-values (all-args _s) (rev-take peekn stack* args))
  (make-nt-token nt (apply action-fun all-args) args))

(define (rev-take n xs acc)
  (let loop ([n n] [xs xs] [acc acc])
    (cond [(zero? n) (values acc xs)]
          [else (loop (sub1 n) (cdr xs) (cons (car xs) acc))])))

;; ----------------------------------------

(struct ll1-context (op stack)
  #:methods gen:context
  [(define (context->stack self)
     (ll1-context-stack self))
   (define (context->stacks self)
     (list (context->stack self)))
   (define (context->expected-terminals self)
     (match (ll1-context-op self)
       [(list 'nt nt ts) ts]
       [(list 't t) (list t)]
       [(list 'top t) #f]))])
