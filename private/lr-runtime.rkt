#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

;; A StateTopStack is (cons PState ValueTopStack)
;; A ValueTopStack is null or (cons Token StateTopStack)
;; -- note, reduced nonterminals are also stored as Tokens

(define (lr-parse states vals tz)
  (define DEBUG? #f)
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (vector-ref vals n))
  (define (get-token peek? tr stack)
    (cond [(symbol? (car tr))
           (tz peek? (car tr) (get-token-args (cdr tr) stack))]
          [(eq? (car tr) '#:apply)
           (apply->token (get-val (caddr tr)) (get-token-args (cdddr tr) stack))]
          [else (error 'lr-parse "bad tr: ~e" tr)]))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        [(list datum) datum]
        [(? exact-nonnegative-integer? index)
         (tok-v (list-ref stack (+ index index -1)))])))

  (define (loop stack)
    (define st (car stack))
    (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st)))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (tok-v (cadr (cddr stack)))]
                  [(virtual) (tok-v (cadr stack))]))]
          [(pstate-lookahead st)
           => (lambda (lookahead)
                (define next-tok (get-token #t (pstate-tr st) stack))
                (cond [(hash-ref lookahead (tok-t next-tok) #f)
                       => (lambda (reds) (reduce st stack (car reds)))]
                      [else (shift st stack)]))]
          [(pair? (pstate-reduce st))
           (reduce st stack (car (pstate-reduce st)))]
          [else (shift st stack)]))

  (define (reduce st stack red)
    (match-define (reduction nt index arity action) red)
    (define-values (args stack*) (pop-values arity stack))
    (define value (tok nt (apply (get-val action) args)))
    (when DEBUG? (eprintf "REDUCE: ~v\n" value))
    (goto value stack*))

  (define (shift st stack)
    (define next-tok (get-token #f (pstate-tr st) stack))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (loop (list* (get-state next-state) next-tok stack)))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (loop (list* (get-state next-state) next-tok stack)))]
          [else (error 'lr-parse "next = ~v, state = ~v" next-tok (car stack))]))

  (define (goto reduced stack)
    (define st (car stack))
    (when DEBUG? (eprintf "RETURN VIA #~s\n" (pstate-index st)))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    (when DEBUG? (eprintf "GOTO ~v\n" next-state))
    (loop (list* (get-state next-state) reduced stack)))

  (loop (list (get-state 0))))

(define (pop-values arity stack) ;; produces values in original order
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))

(define (apply->token f args)
  (define v (apply f args))
  (list (if (ok-terminal? v) v 'bad-token-name)))

(define (ok-terminal? v)
  (or (symbol? v) (char? v) (boolean? v) (exact-integer? v)))
