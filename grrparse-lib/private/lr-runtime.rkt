;; Copyright 2019-2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
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
  (define (get-val n) (if (eq? n 'accept) values (vector-ref vals n)))

  (match-define (tokenizer tz-get-token tz-commit-last) tz)

  (define (get-token tr stack)
    (cond [(symbol? tr)
           (tz-get-token tr null)]
          [(pair? tr)
           (tz-get-token (car tr) (eval-user-expr (cdr tr) stack))]
          [else (error 'lr-parse "bad token reader: ~e" tr)]))
  (define (eval-user-expr ue stack)
    (apply (get-val (expr:user-fun ue))
           (get-token-args (expr:user-args ue) stack)))
  (define (get-token-args args stack)
    (for/list ([vi (in-list args)])
      (list-ref stack (+ vi vi 1))))

  (define (loop stack next-tok)
    (loop* (car stack) stack next-tok))
  (define (loop* st stack next-tok)
    ;; st is usually (car stack), except when forwarded by #:top
    (dprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(pstate-lookahead st)
           => (lambda (lookahead)
                (let ([next-tok (or next-tok (get-token (pstate-tr st) stack))])
                  (cond [(hash-ref lookahead (token-name next-tok) #f)
                         => (lambda (reds) (reduce stack (car reds) next-tok))]
                        [else (shift st stack next-tok)])))]
          ;; ??? reduce vs shift priority?
          [(pair? (pstate-reduce st))
           (reduce stack (car (pstate-reduce st)) next-tok)]
          [else (shift st stack next-tok)]))

  (define (reduce stack red next-tok)
    (match-define (reduction nt index arity ctxn action) red)
    (cond [(eq? action 'accept)
           ;; If no lookahead, then last token was shifted, so commit.
           (when (not next-tok) (tz-commit-last))
           (cadr stack)]
          [else
           (define-values (stack* args all-args) (pop/peek-values arity ctxn stack))
           (define (mktok v) (make-nt-token nt v args))
           (define value (apply (get-val action) all-args))
           (cond [(action:reject? value)
                  (fail 'reduce stack* (mktok value) next-tok)]
                 [(action:collect? value)
                  (mktok (collect-box (list value)))]
                 [else
                  (dprintf "REDUCE: ~v\n" value)
                  (goto (mktok value) stack* next-tok)])]))

  (define (shift st stack next-tok)
    (match (pstate-tr st)
      ['#:top (do-top st stack next-tok)]
      [tr (shift* st stack (or next-tok (get-token tr stack)))]))

  (define (do-top st stack next-tok)
    (define last-tok-value (token-value* (cadr stack) '#:else))
    (define shift-h (pstate-shift st))
    (cond [(or (hash-ref shift-h last-tok-value #f)
               (hash-ref shift-h '#:else #f))
           => (lambda (next-state)
                (dprintf "TOP ~v, #~s\n" last-tok-value next-state)
                (loop* (get-state next-state) stack next-tok))]
          [else (fail 'top stack (token last-tok-value) next-tok)]))

  (define (shift* st stack next-tok) ;; PRE: next-tok != #f
    (cond [(hash-ref (pstate-shift st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (loop (list* (get-state next-state) next-tok stack) #f))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (token-name next-tok) #f)
           => (lambda (next-state)
                (loop (list* (get-state next-state) next-tok stack) #f))]
          [else (fail 'shift stack next-tok next-tok)]))

  (define (goto reduced stack next-tok)
    (define st (car stack))
    (dprintf "RETURN TO #~s\n" (pstate-index st))
    (define next-state (hash-ref (pstate-goto st) (token-name reduced)))
    (dprintf "GOTO #~s\n" next-state)
    (loop (list* (get-state next-state) reduced stack) next-tok))

  (define (fail how stack value next-tok)
    ;; If no lookahead, then last token was shifted, so commit.
    (when (not next-tok) (tz-commit-last))
    (parse-error 'lr-parser (lr-context how (cons value stack))))

  (loop (list (get-state 0)) #f))

;; ----------------------------------------

(define (pop/peek-values popn peekn xs) ;; produces values in original order
  (let loop ([popn popn] [xs xs] [acc null])
    (cond [(zero? popn) (values xs acc (peek-values peekn xs acc))]
          [else (loop (sub1 popn) (cddr xs) (cons (cadr xs) acc))])))

(define (peek-values n xs acc)
  (let loop ([n n] [xs xs] [acc acc])
    (if (zero? n) acc (loop (sub1 n) (cddr xs) (cons (cadr xs) acc)))))
