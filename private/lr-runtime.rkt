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
    (loop* (car stack) stack))
  (define (loop* st stack)
    ;; st is usually (car stack), except when forwarded by #:top
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
                      [else (shift st stack)]))]
          ;; ??? reduce vs shift priority?
          [(pair? (pstate-reduce st))
           (reduce stack (car (pstate-reduce st)))]
          [else (shift st stack)]))

  (define (reduce stack red)
    (match-define (reduction nt index arity ctxn action) red)
    (define-values (stack* args all-args) (pop/peek-values arity ctxn stack))
    (define value (make-nt-token nt (apply (get-val action) all-args) args))
    (cond [(filter:reject? (token-value* value))
           (fail 'reduce stack value)]
          [else
           (dprintf "REDUCE: ~v\n" nt value)
           (goto value stack*)]))

  (define (shift st stack)
    (match (pstate-tr st)
      ['#:top (do-top st stack)]
      [tr (shift* st (get-token #f tr stack) stack)]))

  (define (do-top st stack)
    (define last-tok-value (token-value* (cadr stack) '#:else))
    (define shift-h (pstate-shift st))
    (cond [(or (hash-ref shift-h last-tok-value #f)
               (hash-ref shift-h '#:else #f))
           => (lambda (next-state)
                (dprintf "TOP ~v, #~s\n" last-tok-value next-state)
                (loop* (get-state next-state) stack))]
          [else (fail 'top stack (token last-tok-value))]))

  (define (shift* st next-tok stack)
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
    (parse-error 'lr-parser (lr-context how (cons next-tok stack))))

  (loop (list (get-state 0))))

;; ----------------------------------------

(define (pop/peek-values popn peekn xs) ;; produces values in original order
  (let loop ([popn popn] [xs xs] [acc null])
    (cond [(zero? popn) (values xs acc (peek-values peekn xs acc))]
          [else (loop (sub1 popn) (cddr xs) (cons (cadr xs) acc))])))

(define (peek-values n xs acc)
  (let loop ([n n] [xs xs] [acc acc])
    (if (zero? n) acc (loop (sub1 n) (cddr xs) (cons (cadr xs) acc)))))

;; ----------------------------------------

(struct lr-context (op vsk)
  #:methods gen:context
  [(define (context->stack self)
     (define (convert v)
       (if (pstate? v) (pretty-state (pstate-index v) (pstate-label v)) v))
     (map convert (lr-context-vsk self)))
   (define (context->stacks self)
     (list (context->stack self)))
   (define (context->expected-terminals self)
     (match-define (lr-context op (list* v1 s2 _)) self)
     (if (eq? op 'top) #f (hash-keys (pstate-shift s2))))])
