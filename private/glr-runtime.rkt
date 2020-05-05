#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

;; A TStack is one of
;; - null
;; - (cons X TStack)
;; - (TJoin (NonemptyListof TStack)) -- TJoin should not contain TJoin child
;; A TStack represents a list of stacks with tail sharing (standard
;; for cons-based lists) *and* head sharing (via TJoin).
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

;; ============================================================

;; A StStack has a State at the top, followed by a VStack (or empty).

;; A VStack has a Value (a token or reduced nonterminal) at the top,
;; followed by a StStack.

;; conventions:
;; - sk, sk** : StStack
;; - vsk*     : VStack

(define (with-tstack-pop-values sk arity k)
  ;;(check-state-at-top? 'with-stack-pop-values sk)
  (let loop ([sk sk] [arity arity] [acc null])
    (cond [(zero? arity) (k sk acc)]
          [else (with-tstack-look sk 2
                  (lambda (s1 v2 sk**) (loop sk** (sub1 arity) (cons v2 acc))))])))

;; ----------------------------------------

(define (glr-parse states vals tz #:mode [mode 'complete])
  (define DEBUG? #f)
  (define KEEP-FAIL? #f)
  (define-syntax-rule (push! var value) (set! var (cons value var)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (vector-ref vals n))

  (define (get-next-token tr)
    ;; FIXME: for now, just support no-argument token-readers
    ;; FIXME: no need to treat '#:apply as actual token read! no need to sync!
    (match tr
      [(cons (? symbol? tk) '()) (tz #f tk null)]
      [_ (error 'glr-parse "unsupported token reader: ~e" tr)]))

  (define failed null) ;; mutated; (Listof VStack)
  (define ready null) ;; mutated; (Listof StStack); ready to look at next token
  (define done null) ;; mutated; (Listof Result)

  ;; run-until-look : StStack ?? -> Void
  ;; runs and adds to ready
  (define (run-until-look sk next-tok)
    ;;(check-state-at-top? 'run-until-look/1 ssk)
    (with-tstack-look sk 1
      (lambda (st vsk*)
        (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (pstate-index st) (pstate-label st)))
        (cond [(and (eq? (pstate-accept st) 'true))
               ;; we got here by a shift; discard shift and return state
               ;;(check-value-at-top? 'run-until-look/2 vsk*)
               (with-tstack-look vsk* 3
                 (lambda (v1 s2 v3 sk**) (push! done (tok-v v3))))]
              [(and (eq? (pstate-accept st) 'virtual) (memq mode '(first-done)))
               ;;(check-value-at-top? 'run-until-look/3 vsk*)
               ;; we got here by a goto; result is first value
               (with-tstack-look vsk* 1
                 (lambda (v1 sk**) (push! done (tok-v v1))))]
              [(pstate-lookahead st)
               => (lambda (lookahead)
                    (cond [next-tok (look* st vsk* next-tok)]
                          [else (push! ready (cons st vsk*))]))]
              [else
               (for ([red (pstate-reduce st)])
                 (reduce st vsk* red next-tok))
               (cond [next-tok (look* st vsk* next-tok)]
                     [(not (hash-empty? (pstate-shift st)))
                      (push! ready (cons st vsk*))])]))))

  (define (reduce st vsk* red next-tok)
    ;;(check-value-at-top? 'reduce/1 vsk*)
    (match-define (reduction nt index arity action) red)
    (with-tstack-pop-values (cons st vsk*) arity
      (lambda (sk** args)
        ;;(check-state-at-top? 'reduce/2 sk**)
        (define value (apply (get-val action) args))
        (when DEBUG? (eprintf "REDUCE(~s): ~v\n" nt value))
        (cond [(filter:reject? value)
               (when KEEP-FAIL? (push! failed (cons (tok nt value) sk**)))]
              [else (goto (tok nt value) sk** next-tok)]))))

  (define (look sks) ;; sks : (Listof StStack), each stack starts with cons
    (define tr (pstates-consistent-tr (map car sks)))
    (define next-tok (get-next-token tr))
    (for ([sk (in-list sks)])
      (match-define (cons st vsk*) sk)
      (look* st vsk* next-tok)))
  (define (look* st vsk* next-tok)
    ;;(check-value-at-top? 'shift/1 vsk*)
    (define reds (hash-ref (or (pstate-lookahead st) #hash()) (tok-t next-tok) null))
    (for ([red (in-list reds)]) (reduce st vsk* red next-tok))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (run-until-look (list* (get-state next-state) next-tok st vsk*) #f))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (run-until-look (list* (get-state next-state) next-tok st vsk*) #f))]
          [(null? reds)
           (when KEEP-FAIL? (push! failed (list* next-tok st vsk*)))]))

  (define (goto reduced sk next-tok)
    (with-tstack-look sk 1
      (lambda (st vsk*)
        ;;(check-value-at-top? 'goto/1 vsk*)
        (when DEBUG? (eprintf "RETURN VIA #~s\n" (pstate-index st)))
        (define next-state (hash-ref (pstate-goto st) (car reduced)))
        (when DEBUG? (eprintf "GOTO ~v\n" next-state))
        (run-until-look (list* (get-state next-state) reduced st vsk*) next-tok))))

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
