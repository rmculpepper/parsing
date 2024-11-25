;; Copyright 2019-2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/vector
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

  (define INIT-STACK 25)
  (define stack (make-vector INIT-STACK #f))
  (define stack-next 0)
  (define (stack-ref k) (vector-ref stack (- stack-next k 1)))
  (define (stack-ensure-capacity cap)
    (unless (<= cap (vector-length stack))
      (define new-stack (make-vector (* 2 (vector-length stack)) #f))
      (vector-copy! new-stack 0 stack 0 stack-next)
      (set! stack new-stack)
      (when #f ;; can omit if INIT-STACK greater than length of any single push
        (stack-ensure-capacity cap))))
  (define stack-push! ;; leftmost first!
    (case-lambda
      [(arg)
       (stack-ensure-capacity (+ stack-next 1))
       (vector-set! stack stack-next arg)
       (set! stack-next (+ stack-next 1))]
      [(arg1 arg2)
       (stack-ensure-capacity (+ stack-next 2))
       (vector-set! stack (+ stack-next 0) arg1)
       (vector-set! stack (+ stack-next 1) arg2)
       (set! stack-next (+ stack-next 2))]))
  (define (stack-pop k)
    (set! stack-next (- stack-next k))
    (for ([i (in-range stack-next (+ stack-next k))])
      (vector-set! stack i #f)))
  (define (pop/peek-values popn peekn)
    ;; produces values in original order; PRE: top of stack is state
    (let loop ([popn popn] [k 0] [acc null])
      (cond [(zero? popn)
             (begin0 (values acc (peek-values peekn k acc))
               (stack-pop k))]
            [else
             (loop (sub1 popn) (+ k 2) (cons (stack-ref (+ k 1)) acc))])))
  (define (peek-values n k acc)
    (let loop ([n n] [k k] [acc acc])
      (if (zero? n) acc (loop (sub1 n) (+ k 2) (cons (stack-ref (+ k 1)) acc)))))

  (define (get-state n) (vector-ref states n))
  (define (get-val n) (if (eq? n 'accept) values (vector-ref vals n)))

  (match-define (tokenizer tz-get-token tz-commit-last) tz)

  (define (get-token tr)
    (cond [(symbol? tr)
           (tz-get-token tr null)]
          [(pair? tr)
           (tz-get-token (car tr) (eval-user-expr (cdr tr)))]
          [else (error 'lr-parse "bad token reader: ~e" tr)]))
  (define (eval-user-expr ue)
    (apply (get-val (expr:user-fun ue))
           (get-token-args (expr:user-args ue))))
  (define (get-token-args args)
    (for/list ([arg (in-list args)])
      (match arg
        [(? exact-nonnegative-integer? index)
         (stack-ref (+ index index 1))])))

  (define (loop next-tok)
    (loop* (stack-ref 0) next-tok))
  (define (loop* st next-tok)
    ;; st is usually (stack-ref 0), except when forwarded by #:top
    (dprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(pstate-lookahead st)
           => (lambda (lookahead)
                (let ([next-tok (or next-tok (get-token (pstate-tr st)))])
                  (cond [(hash-ref lookahead (token-name next-tok) #f)
                         => (lambda (reds) (reduce (car reds) next-tok))]
                        [else (shift st next-tok)])))]
          ;; ??? reduce vs shift priority?
          [(pair? (pstate-reduce st))
           (reduce (car (pstate-reduce st)) next-tok)]
          [else (shift st next-tok)]))

  (define (reduce red next-tok)
    (match-define (reduction nt index arity ctxn action) red)
    (cond [(eq? action 'accept)
           (when (not next-tok) (tz-commit-last))
           (stack-ref 1)]
          [else
           (define-values (args all-args) (pop/peek-values arity ctxn))
           (define (mktok v) (make-nt-token nt v args))
           (define value (apply (get-val action) all-args))
           (cond [(action:reject? value)
                  (fail 'reduce (mktok value) next-tok)]
                 [(action:collect? value)
                  (mktok (collect-box (list value)))]
                 [else
                  (dprintf "REDUCE: ~v\n" value)
                  (goto (mktok value) next-tok)])]))

  (define (shift st next-tok)
    (match (pstate-tr st)
      ['#:top (do-top st next-tok)]
      [tr (shift* st (or next-tok (get-token tr)))]))

  (define (do-top st next-tok)
    (define last-tok-value (token-value* (stack-ref 1) '#:else))
    (define shift-h (pstate-shift st))
    (cond [(or (hash-ref shift-h last-tok-value #f)
               (hash-ref shift-h '#:else #f))
           => (lambda (next-state)
                (dprintf "TOP ~v, #~s\n" last-tok-value next-state)
                (loop* (get-state next-state) next-tok))]
          [else (fail 'top (token last-tok-value) next-tok)]))

  (define (shift* st next-tok) ;; PRE: next-tok != false
    (cond [(hash-ref (pstate-shift st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (define st (get-state next-state))
                (stack-push! next-tok st)
                (loop* st #f))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (token-name next-tok) #f)
           => (lambda (next-state)
                (define st (get-state next-state))
                (stack-push! next-tok st)
                (loop* st #f))]
          [else (fail 'shift next-tok next-tok)]))

  (define (goto reduced next-tok)
    (define st (stack-ref 0))
    (dprintf "RETURN VIA #~s\n" (pstate-index st))
    (define next-state (hash-ref (pstate-goto st) (token-name reduced)))
    (dprintf "GOTO ~v\n" next-state)
    (define next-st (get-state next-state))
    (stack-push! reduced next-st)
    (loop* next-st next-tok))

  (define (fail how value next-tok)
    (when (not next-tok) (tz-commit-last))
    (define stack-as-list
      (reverse (vector->list (vector-copy stack 0 stack-next))))
    (parse-error 'lr-parser (lr-context how (cons value stack-as-list))))

  (let ([st0 (get-state 0)])
    (stack-push! st0)
    (loop* st0 #f)))
