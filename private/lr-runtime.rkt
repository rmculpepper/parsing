#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

;; StStack = (cons PState VStack)
;; VStack = (cons Token StStack) | null
;; -- Note: reduced nonterminals are also stored as Tokens

;; conventions:
;; - stack : StStack
;; - vsk*  : VStack

(define (lr-parse states vals tz)
  (define DEBUG? #f)
  (define-syntax-rule (dprintf fmt arg ...) (when DEBUG? (eprintf fmt arg ...)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (vector-ref vals n))

  (define (get-token peek? tr stack)
    (cond [(symbol? tr)
           (tz peek? tr null)]
          [(pair? tr)
           (tz peek? (car tr) (eval-user-expr (cdr tr) stack))]
          [else (error 'lr-parse "bad tr: ~e" tr)]))
  (define (eval-user-expr ue stack)
    (apply (get-val (expr:user-fun ue))
           (get-token-args (expr:user-args ue) stack)))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        ;;[(list datum) datum] ;; convert to token?
        [(? exact-nonnegative-integer? index)
         (list-ref stack (+ index index 1))])))

  (define (loop stack)
    (define st (car stack))
    (dprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (cadr (cddr stack))]
                  [(virtual) (cadr stack)]))]
          [(pstate-lookahead st)
           => (lambda (lookahead)
                (define next-tok (get-token #t (pstate-tr st) stack))
                (cond [(hash-ref lookahead (token-name next-tok) #f)
                       => (lambda (reds) (reduce stack (car reds)))]
                      [else (shift stack)]))]
          ;; ??? reduce vs shift priority?
          [(pair? (pstate-reduce st))
           (reduce stack (car (pstate-reduce st)))]
          [else (shift stack)]))

  (define (reduce stack red)
    (match-define (reduction nt index arity action) red)
    (define-values (args stack*) (pop-values arity stack))
    (define value (make-nt-token nt (apply (get-val action) args) args))
    (cond [(filter:reject? (token-value* value))
           (fail 'reduce stack value)]
          [else
           (dprintf "REDUCE: ~v\n" nt value)
           (goto value stack*)]))

  (define (shift stack)
    (define st (car stack))
    (define next-tok (get-token #f (pstate-tr st) stack))
    (cond [(hash-ref (pstate-shift st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (loop (list* (get-state next-state) next-tok stack)))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (token-name next-tok) #f)
           => (lambda (next-state)
                (loop (list* (get-state next-state) next-tok stack)))]
          [else (fail 'shift stack next-tok)]))

  (define (goto reduced stack)
    (define st (car stack))
    (dprintf "RETURN VIA #~s\n" (pstate-index st))
    (define next-state (hash-ref (pstate-goto st) (token-name reduced)))
    (dprintf "GOTO ~v\n" next-state)
    (loop (list* (get-state next-state) reduced stack)))

  (define (fail how stack next-tok)
    (error 'lr-parse "~s ~v, state = ~v" how next-tok (car stack)))

  (loop (list (get-state 0))))

;; ----------------------------------------

(define (pop-values arity stack) ;; produces values in original order
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))

(define (apply->token f args)
  (define v (apply f args))
  (list (if (ok-terminal? v) v 'bad-token-name)))
