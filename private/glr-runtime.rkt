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

(define-syntax with-tstack
  (syntax-rules ()
    [(with-tstack tsk (tsk-var) . body)
     (let ([tsk-var tsk]) . body)]
    [(with-tstack tsk (elem-var . more) . body)
     (tstack-split tsk (lambda (elem-var tsk-rest) (with-tstack tsk-rest more . body)))]))

(define (tstack-split tsk f)
  (match tsk
    [(cons x tsk*) (f x tsk*)]
    [(TJoin tsks) (for ([tsk (in-list tsks)]) (tstack-split tsk f))]
    ['() (error 'tstack-split "empty stack")]))

;; tjoin-on-cdrs : (NEListof TStack) -> (NEListof TStack)
;; The input stacks must be pairs (ie, not empty, not TJoins), and
;; each stack in the result is also a pair.
(define (tjoin-on-cdrs stacks)
  (match stacks
    [(list _) stacks]
    [(list s1 s2)
     (cond [(eq? (car s1) (car s2))
            (list (cons (car s1) (TJoin (list (cdr s1) (cdr s2)))))]
           [else stacks])]
    [else
     (for/list ([group (in-list (group-by car stacks eq?))])
       (cond [(singleton? group) (car group)]
             [else (cons (caar group) (TJoin (map cdr group)))]))]))

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

;; stacks-consistent-tr : (Listof StStack) -> TR
;; FIXME: in GLR, if token arguments, *values* must be consistent, not just exprs
;; For now, just disallow parameters.
(define (stacks-consistent-tr sks)
  (define (safe-tr? x) (match x [(? symbol?) #t] [_ #f]))
  (match sks
    #;[(list sk) (pstate-tr (car sk))] ;; unsound: if params, rest of stack may be tjoin
    [sks
     (for/fold ([seen-tr #f]) ([sk (in-list sks)])
       (define tr (pstate-tr (car sk)))
       (cond [(or (equal? tr seen-tr) (eq? tr #f)) seen-tr]
             [(safe-tr? tr)
              (cond [(or (eq? seen-tr #f) (equal? seen-tr tr)) tr]
                    [else (error 'glr-parse "ambiguous token reader\n  candidates: ~e"
                                 (remove-duplicates (map pstate-tr (map car sks))))])]
             [else (error 'glr-parse "unsupported token reader\n  reader: ~e" tr)]))]))

;; ============================================================

;; A StStack has a State at the top, followed by a VStack (or empty).

;; A VStack has a Value (a token or reduced nonterminal) at the top,
;; followed by a StStack.

;; conventions:
;; - sk, sk** : StStack
;; - vsk*     : VStack

(define (with-tstack-pop/peek-values sk popn peekn k)
  (let loop ([sk sk] [popn popn] [acc null])
    (cond [(zero? popn) (with-tstack-peek-values sk peekn acc k)]
          [else (with-tstack sk [s1 v2 sk**]
                  (loop sk** (sub1 popn) (cons v2 acc)))])))

(define (with-tstack-peek-values sk peekn onto k)
  (define (rev-append xs ys) (foldl cons ys xs))
  (let loop ([sk sk] [peekn peekn] [acc onto] [revsk null])
    (cond [(zero? peekn) (k (rev-append revsk sk) onto acc)]
          [else (with-tstack sk [s1 v2 sk**]
                  (loop sk** (sub1 peekn) (cons v2 acc) (list* v2 s1 revsk)))])))

;; ----------------------------------------

(define (glr-parse states vals tz #:mode [mode 'complete])
  (define DEBUG? #f)
  (define KEEP-FAIL? #f)
  (define-syntax-rule (push! var value) (set! var (cons value var)))
  (define-syntax-rule (dprintf fmt arg ...) (when DEBUG? (eprintf fmt arg ...)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (vector-ref vals n))

  (define (get-next-token tr)
    ;; FIXME: for now, just support no-argument token-readers
    ;; FIXME: no need to treat '#:apply as actual token read! no need to sync!
    (match tr
      [(? symbol? tk) (tz #f tk null)]
      [_ (error 'glr-parse "unsupported token reader: ~e" tr)]))

  (define failed null) ;; mutated; (Listof VStack)
  (define ready null) ;; mutated; (Listof StStack); ready to look at next token
  (define done null) ;; mutated; (Listof Result)

  ;; run-until-look : StStack ?? -> Void
  ;; Runs each "thread" in sk until it needs input and adds to ready
  ;; list. A thread may fork or fail.
  (define (run-until-look sk next-tok)
    ;;(check-state-at-top? 'run-until-look/1 sk)
    (with-tstack sk [st vsk*] (run-until-look* st vsk* next-tok)))
  (define (run-until-look* st vsk* next-tok)
    (dprintf "\nR2L STATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(and (eq? (pstate-accept st) 'true))
           ;; we got here by a shift; discard shift and return state
           ;;(check-value-at-top? 'run-until-look/2 vsk*)
           (dprintf "-- R2L #~s accept\n" (pstate-index st))
           (with-tstack vsk* [v1 s2 v3 sk**]
             (push! done v3))]
          [(and (eq? (pstate-accept st) 'virtual) (memq mode '(first-done)))
           ;;(check-value-at-top? 'run-until-look/3 vsk*)
           ;; we got here by a goto; result is first value
           (dprintf "-- R2L #~s virtual accept\n" (pstate-index st))
           (with-tstack vsk* [v1 sk**]
             (push! done v1))]
          [(pstate-lookahead st)
           => (lambda (lookahead)
                (cond [next-tok
                       (dprintf "-- R2L #~s lookahead (~v)\n"
                                (pstate-index st) (token-name next-tok))
                       (look* #f st vsk* next-tok)]
                      [else (push! ready (cons st vsk*))]))]
          [else
           (for ([red (pstate-reduce st)] [i (in-naturals)])
             (dprintf "-- R2L #~s reduction ~s/~s\n"
                      (pstate-index st) (add1 i) (length (pstate-reduce st)))
             (reduce st vsk* red next-tok))
           (dprintf "-- R2L #~s continue (~s)\n"
                    (pstate-index st) (and next-tok (token-name next-tok)))
           (cond [next-tok (look* #f st vsk* next-tok)]
                 [(not (hash-empty? (pstate-shift st)))
                  (push! ready (cons st vsk*))])]))

  (define (reduce st vsk* red next-tok)
    ;;(check-value-at-top? 'reduce/1 vsk*)
    (match-define (reduction nt index arity ctxn action) red)
    (with-tstack-pop/peek-values (cons st vsk*) arity ctxn
      (lambda (sk** args all-args)
        (define value (make-nt-token nt (apply (get-val action) all-args) args))
        (dprintf "REDUCE(~s): ~v\n" nt value)
        (cond [(filter:reject? (token-value* value))
               (when KEEP-FAIL? (push! failed (cons value sk**)))]
              [else (goto value sk** next-tok)]))))

  (define (look sks) ;; sks : (Listof StStack), each stack starts with cons
    (define tr (stacks-consistent-tr sks))
    (define next-tok (get-next-token tr))
    (dprintf "LOOK: read ~v\nSTATES: ~v\n\n"
             next-tok (map pstate-index (map car sks)))
    (for ([sk (in-list sks)])
      (match-define (cons st vsk*) sk)
      (look* sk st vsk* next-tok)))
  (define (look* sk st vsk* next-tok) ;; sk = (cons st vsk*) or #f, saves re-alloc
    ;;(check-value-at-top? 'shift/1 vsk*)
    (define reds (hash-ref (or (pstate-lookahead st) #hash()) (token-name next-tok) null))
    (for ([red (in-list reds)] [i (in-naturals)])
      (dprintf "-- L #~s reduction ~s/~s\n" (pstate-index st) (add1 i) (length reds))
      (reduce st vsk* red next-tok))
    (dprintf "-- L #~s continue (~v)\n" (pstate-index st) (token-name next-tok))
    (cond [(hash-ref (pstate-shift st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (let ([sk (or sk (cons st vsk*))])
                  (run-until-look* (get-state next-state) (list* next-tok sk) #f)))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (let ([sk (or sk (cons st vsk*))])
                  (run-until-look* (get-state next-state) (list* next-tok sk) #f)))]
          [(null? reds)
           (when KEEP-FAIL? (push! failed (list* next-tok st vsk*)))]))

  (define (goto reduced sk next-tok)
    (with-tstack sk [st vsk*]
      ;;(check-value-at-top? 'goto/1 vsk*)
      (dprintf "RETURN VIA #~s\n" (pstate-index st))
      (define next-state (hash-ref (pstate-goto st) (token-name reduced)))
      (dprintf "GOTO ~v\n" next-state)
      (run-until-look* (get-state next-state) (list* reduced st vsk*) next-tok)))

  (define (run-all-ready)
    (dprintf "\n==== STEP ====\n")
    (define ready* (tjoin-on-cdrs ready))
    (set! ready null)
    (when #t (set! failed null))
    (look ready*))

  (run-until-look* (get-state 0) null #f)
  (dprintf "\n==== START STEPPING ====\n")
  (let loop ()
    (cond [(and (memq mode '(first-done)) (pair? done)) done]
          [(null? ready) done]
          [else (run-all-ready) (loop)])))
