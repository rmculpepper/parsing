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

;; stacks-consistent-tr : (Listof StStack) -> TRValue
;; where TRValue is Symbol | (cons Symbol List)
;; FIXME: in GLR, if token arguments, *values* must be consistent, not just exprs
;; For now, just disallow parameters.
(define (stacks-consistent-tr sks get-val)
  ;; Note: a single state might have multiple values due to tjoin
  (define seen-tr #f)
  (define (bad tr)
    (error 'glr-parse "ambiguous token reader\n  candidates: ~e, ~e (maybe others)"
           seen-tr tr))
  (for ([sk (in-list sks)])
    (match (pstate-tr (car sk))
      [#f (void)]
      [(? symbol? tr)
       (cond [(not seen-tr) (set! seen-tr tr)]
             [(eqv? tr seen-tr) (void)]
             [else (bad tr)])]
      [(cons tr-name (expr:user fun-index args))
       (define (do-tr tr)
         (cond [(not seen-tr) (set! seen-tr tr)]
               [(and (list? seen-tr) (andmap eqv? tr seen-tr)) (void)]
               [else (bad tr)]))
       (define (argsloop args sk depth acc)
         (cond [(null? args)
                (do-tr (cons tr-name (apply (get-val fun-index) (reverse acc))))]
               [else
                (with-tstack sk [_s v sk**]
                  (cond [(< depth (car args))
                         (argsloop args sk** (add1 depth) acc)]
                        [(= depth (car args))
                         (argsloop (cdr args) sk** (add1 depth) (cons v acc))]
                        [else (error 'stacks-consistent-tr "internal error: out of order")]))]))
       (argsloop args sk 0 null)]))
  (or seen-tr 'default))

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
                  (cond [(no-value? v2)
                         (loop sk** popn acc)]
                        [else
                         (loop sk** (sub1 popn) (cons v2 acc))]))])))

(define (with-tstack-peek-values sk peekn onto k)
  (define (rev-append xs ys) (foldl cons ys xs))
  (let loop ([sk sk] [peekn peekn] [acc onto] [revsk null])
    (cond [(zero? peekn) (k (rev-append revsk sk) onto acc)]
          [else (with-tstack sk [s1 v2 sk**]
                  (cond [(no-value? v2)
                         (loop sk** peekn acc (list* v2 s1 revsk))]
                        [else
                         (loop sk** (sub1 peekn) (cons v2 acc) (list* v2 s1 revsk))]))])))

;; Unlike lr-parse, glr-parse pushes a special "no-value" on top-elem.
;; Must filter out on peek/pop.
(define no-value (void))
(define (no-value? x) (void? x))

(define-syntax-rule (push! var value) (set! var (cons value var)))

;; ----------------------------------------

(define (glr-parse states vals tz #:mode [mode 'complete])
  (define DEBUG? #f)
  (define KEEP-FAIL? #t)
  (define-syntax-rule (dprintf fmt arg ...) (when DEBUG? (eprintf fmt arg ...)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (if (eq? n 'accept) values (vector-ref vals n)))

  (define (get-next-token tr)
    (match tr
      [(? symbol? tk) (tz #f tk null)]
      [(cons tf args) (tz #f tf args)]))

  (define failed null) ;; mutated; (Listof (U VStack (box VStack))) -- box means #:top failed
  (define ready null) ;; mutated; (Listof StStack); ready to look at next token
  (define done null) ;; mutated; (Listof Result)

  ;; run-until-look : StStack ?? -> Void
  ;; Runs each "thread" in sk until it needs input and adds to ready
  ;; list. A thread may fork or fail.
  (define (run-until-look sk next-tok)
    (with-tstack sk [st vsk*] (run-until-look* st vsk* next-tok)))
  (define (run-until-look* st vsk* next-tok)
    (dprintf "\nR2L STATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(pstate-lookahead st)
           => (lambda (lookahead)
                (cond [next-tok
                       (dprintf "-- R2L #~s lookahead (~v)\n"
                                (pstate-index st) (token-name next-tok))
                       (look* #f st vsk* next-tok)]
                      [else (push! ready (cons st vsk*))]))]
          [(eq? (pstate-tr st) '#:top)
           (with-tstack vsk* [v1 sk**]
             (cond [(or (hash-ref (pstate-shift st) (token-value* v1 '#:else) #f)
                        (hash-ref (pstate-shift st) '#:else #f))
                    => (lambda (next-state)
                         (run-until-look (list* (get-state next-state) no-value st v1 sk**) next-tok))]
                   [else (push! failed (box (list* v1 st vsk*)))]))]
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
    (match-define (reduction nt index arity ctxn action) red)
    (cond [(eq? action 'accept)
           (with-tstack vsk* [v sk**]
             (push! done v))]
          [else
           (with-tstack-pop/peek-values (cons st vsk*) arity ctxn
             (lambda (sk** args all-args)
               (define value (make-nt-token nt (apply (get-val action) all-args) args))
               (dprintf "REDUCE(~s): ~v\n" nt value)
               (cond [(filter:reject? (token-value* value))
                      (when KEEP-FAIL? (push! failed (cons value sk**)))]
                     [else (goto value sk** next-tok)])))]))

  (define (look sks) ;; sks : (Listof StStack), each stack starts with cons
    (define tr (stacks-consistent-tr sks get-val))
    (define next-tok (get-next-token tr))
    (dprintf "LOOK: read ~v\nSTATES: ~v\n\n"
             next-tok (map pstate-index (map car sks)))
    (for ([sk (in-list sks)])
      (match-define (cons st vsk*) sk)
      (look* sk st vsk* next-tok)))
  (define (look* sk st vsk* next-tok) ;; sk = (cons st vsk*) or #f, saves re-alloc
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
          [(null? ready) (if (pair? done) done (parse-error 'glr-parser (glr-context failed)))]
          [else (run-all-ready) (loop)])))

;; ----------------------------------------

(struct glr-context (vsks)
  #:transparent
  #:methods gen:context
  [(define (context->stack self)
     (match (context->stacks self)
       [(list s) s] [_ (error 'context->stacks "multiple stacks")]))
   (define (context->stacks self)
     (define stacks null)
     (define (loop tsk acc)
       (if (null? tsk)
           (push! stacks (reverse acc))
           (with-tstack tsk [v tsk*]
             (loop tsk* (cons (convert-pretty-states v) acc)))))
     (for ([vsk (in-list (glr-context-vsks self))])
       (loop (if (box? vsk) (unbox vsk) vsk) null))
     stacks)
   (define (context->expected-terminals self)
     (define h (make-hash))
     (define (loop vsk)
       (with-tstack vsk [v1 s2 _]
         (for ([t (in-hash-keys (pstate-shift s2))]) (hash-set! h t #t))))
     (for ([vsk (in-list (glr-context-vsks self))] #:when (not (box? vsk)))
       (loop vsk))
     (if (hash-empty? h) #f (hash-keys h)))])
