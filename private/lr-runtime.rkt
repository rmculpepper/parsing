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

;; ============================================================

;; A TStack is one of
;; - null
;; - (cons X TStack)
;; - (TJoin (NonemptyListof TStack)) -- TJoin should not contain TJoin child
(struct TJoin (stacks) #:prefab)

;; tstack-look : TStack Nat -> (NonemptyListof TStack)
;; Returns list of tstacks, each of which starts with n conses.
(define (tstack-look tsk n)
  (cond [(zero? n) (list tsk)]
        [else (match tsk
                ['() (error 'tstack-look "empty stack")]
                [(cons x tsk*)
                 (for/list ([tail (in-list (tstack-look tsk* (sub1 n)))])
                   (cons x tail))]
                [(TJoin tsks*)
                 (append* (for/list ([tsk* (in-list tsks*)]) (tstack-look tsk* n)))])]))

(define (with-tstack-look tsk n k)
  ;; (eprintf "with-tstack-look : ~s, ~v\n" n tsk)
  (let loop ([tsk tsk] [n n] [acc null])
    (cond [(zero? n) (apply k (reverse (cons tsk acc)))]
          [else (match tsk
                  ['() (error 'with-tstack-look "empty stack")]
                  [(cons x tsk*)
                   (loop tsk* (sub1 n) (cons x acc))]
                  [(TJoin tsks*)
                   (for ([tsk* (in-list tsks*)]) (loop tsk* n acc))])])))

;; tstack-join : (NonemptyListof TStack) -> TStack
(define (tstack-join tsks) (if (singleton? tsks) (car tsks) (tstack-join* tsks)))

(define (tstack-join* tsks)
  (define join-tsks (filter TJoin? tsks))
  (define cons-tsks (filter cons? tsks))
  (define null-tsks (filter null? tsks))
  (cond [(pair? cons-tsks)
         (define cons-tsk* (tstack-join/cons (group-by car cons-tsks)))
         (tjoin (cons cons-tsk* (append* null-tsks (map TJoin-stacks join-tsks))))]
        [else (tjoin (append* null-tsks (map TJoin-stacks join-tsks)))]))

(define (tstack-join/cons cons-groups)
  (tjoin (for/list ([group (in-list cons-groups)])
           (cond [(singleton? group) (car group)]
                 [else (cons (caar group) (tstack-join (map cdr group)))]))))

(define (tjoin tsks) (if (singleton? tsks) (car tsks) (TJoin tsks)))

(define (singleton? x) (and (pair? x) (null? (cdr x))))

#|
(define (check-state-at-top? who tsk)
  (with-tstack-look tsk 1
    (lambda (st tsk*) (unless (pstate? st) (error who "top of stack is not state: ~v" tsk)))))
(define (check-value-at-top? who tsk)
  (unless (null? tsk)
    (with-tstack-look tsk 1
      (lambda (st tsk*) (unless (pair? st) (error who "top of stack is not value: ~v" tsk))))))
|#

;; ----------------------------------------

(define (glr-parse states vals tz #:mode [mode 'complete])
  (define DEBUG? #f)
  (define-syntax-rule (push! var value) (set! var (cons value var)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (vector-ref vals n))

  (define (get-next-token tr)
    ;; FIXME: for now, just support no-argument token-readers
    ;; FIXME: no need to treat '#:apply as actual token read! no need to sync!
    (match tr
      [(cons (? symbol? tk) '()) (tz #f tk null)]
      [_ (error 'glr-parse "unsupported token reader: ~e" tr)]))

  (define failed null) ;; mutated; (Listof TStack)
  (define ready null) ;; mutated; (Listof TStack); ready to look at next token
  (define done null) ;; mutated; (Listof Result)

  ;; run-until-look : runs and adds to ready
  (define (run-until-look tsk next-tok)
    ;;(check-state-at-top? 'run-until-look/1 tsk)
    (with-tstack-look tsk 1
      (lambda (st tsk*)
        (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st)))
        (cond [(and (eq? (pstate-accept st) 'true))
               ;; we got here by a shift; discard shift and return state
               ;;(check-value-at-top? 'run-until-look/2 tsk*)
               (with-tstack-look tsk* 3
                 (lambda (v1 s2 v3 tsk**) (push! done (tok-v v3))))]
              [(and (eq? (pstate-accept st) 'virtual) (memq mode '(first-done)))
               ;;(check-value-at-top? 'run-until-look/3 tsk*)
               ;; we got here by a goto; result is first value
               (with-tstack-look tsk* 1
                 (lambda (v1 tsk**) (push! done (tok-v v1))))]
              [(pstate-lookahead st)
               => (lambda (lookahead)
                    (cond [next-tok (look* st tsk* next-tok)]
                          [else (push! ready (cons st tsk*))]))]
              [else
               (for ([red (pstate-reduce st)])
                 (reduce st tsk* red next-tok))
               (cond [next-tok (look* st tsk* next-tok)]
                     [(not (hash-empty? (pstate-shift st)))
                      (push! ready (cons st tsk*))])]))))

  (define (reduce st tsk* red next-tok)
    ;;(check-value-at-top? 'reduce/1 tsk*)
    (match-define (reduction nt index arity action) red)
    (with-tstack-pop-values (cons st tsk*) arity
      (lambda (tsk** args)
        ;;(check-state-at-top? 'reduce/2 tsk**)
        (define value (tok nt (apply (get-val action) args)))
        (when DEBUG? (eprintf "REDUCE: ~v\n" value))
        (goto value tsk** next-tok))))

  ;; convention:
  ;; - tsk, tsk**, etc   -- TStack with a state index at top
  ;; - tsk*, tsk***, etc -- TStack with a token at top

  (define (look stacks)
    (define tr (pstates-consistent-tr (map car stacks)))
    (define next-tok (get-next-token tr))
    (for ([stack (in-list stacks)])
      (match-define (cons st tsk*) stack)
      (look* st tsk* next-tok)))
  (define (look* st tsk* next-tok)
    ;;(check-value-at-top? 'shift/1 tsk*)
    (define reds (hash-ref (or (pstate-lookahead st) #hash()) (tok-t next-tok) null))
    (for ([red (in-list reds)]) (reduce st tsk* red next-tok))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (run-until-look (list* (get-state next-state) next-tok st tsk*) #f))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (run-until-look (list* (get-state next-state) next-tok st tsk*) #f))]
          [(null? reds) (push! failed (list* next-tok st tsk*))]))

  (define (goto reduced tsk next-tok)
    (with-tstack-look tsk 1
      (lambda (st tsk*)
        ;;(check-value-at-top? 'goto/1 tsk*)
        (when DEBUG? (eprintf "RETURN VIA #~s\n" (pstate-index st)))
        (define next-state (hash-ref (pstate-goto st) (car reduced)))
        (when DEBUG? (eprintf "GOTO ~v\n" next-state))
        (run-until-look (list* (get-state next-state) reduced st tsk*) next-tok))))

  (define (run-all-ready)
    (when DEBUG? (eprintf "\n==== STEP ====\n"))
    (define ready* ready)
    (set! ready null)
    (when #t (set! failed null))
    (look ready*))

  (run-until-look (list (get-state 0)) #f)
  (let loop ()
    (cond [(and (memq mode '(first-done)) (pair? done)) done]
          [(null? ready) done]
          [else (run-all-ready) (loop)])))

;; FIXME: in GLR, if token arguments, *values* must be consistent, not just exprs
;; For now, just disallow parameters.
(define (pstates-consistent-tr states [fail #f])
  (define proper-states (filter pstate-tr states)) ;; ignore polymorphic
  (match (group-by pstate-tr proper-states)
    [(list) #f]
    [(list group)
     (define tr (pstate-tr (car group)))
     (match tr [(list (? symbol?)) tr] [_ (error 'glr-parse "unsupported token reader: ~e" tr)])]
    [groups
     (define trs (map pstate-tr (map car groups)))
     (if fail (fail trs) (error 'glr-parse "ambiguous token reader\n  candidates: ~e" trs))]))

(define (with-tstack-pop-values tsk arity k)
  ;;(check-state-at-top? 'with-stack-pop-values tsk)
  (let loop ([tsk tsk] [arity arity] [acc null])
    (cond [(zero? arity) (k tsk acc)]
          [else (with-tstack-look tsk 2
                  (lambda (s1 v2 tsk**) (loop tsk** (sub1 arity) (cons v2 acc))))])))
