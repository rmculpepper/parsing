#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

;; StStack = (cons PState VStack)
;; VStack = null | (cons Token StStack) | (Vectorof VStack)
;; where a vector node does not contain vector children.

;; TStack = StStack | VStack, represents a list of stacks with tail
;; sharing (as standard for lists) *and* head sharing (via vectors).

(define-syntax with-ststack
  (syntax-rules ()
    [(with-ststack sk (sk-var) . body)
     (let ([sk-var sk]) . body)]
    [(with-ststack sk (var1 . more) . body)
     (match sk [(cons var1 rest) (with-vstack rest more . body)])]))

(define-syntax with-vstack
  (syntax-rules ()
    [(with-vstack vsk (vsk-var) . body)
     (let ([vsk-var vsk]) . body)]
    [(with-vstack vsk (var1 . more) . body)
     (vstack-split vsk (lambda (var1 rest) (with-vstack rest more . body)))]))

(define (vstack-split tsk f)
  (match tsk
    [(cons x tsk*) (f x tsk*)]
    [(? vector? tsks) (for ([tsk (in-vector tsks)]) (vstack-split tsk f))]
    ['() (error 'tstack-split "empty stack")]))

;; tjoin-on-cdrs : (NEListof StStack) -> (NEListof StStack)
(define (tjoin-on-cdrs stacks)
  (match stacks
    [(list _) stacks]
    [(list s1 s2)
     (cond [(eq? (car s1) (car s2))
            (list (cons (car s1) (vector (cdr s1) (cdr s2))))]
           [else stacks])]
    [else
     (for/list ([group (in-list (group-by car stacks eq?))])
       (cond [(singleton? group) (car group)]
             [else (cons (caar group) (list->vector (map cdr group)))]))]))

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
                (with-ststack sk [_s v sk**]
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

(define (with-ststack-pop/peek-values sk popn peekn k)
  (let loop ([sk sk] [popn popn] [acc null])
    (cond [(zero? popn) (with-ststack-peek-values sk peekn acc k)]
          [else (with-ststack sk [s1 v2 sk**]
                  (cond [(no-value? v2)
                         (loop sk** popn acc)]
                        [else
                         (loop sk** (sub1 popn) (cons v2 acc))]))])))

(define (with-ststack-peek-values sk peekn onto k)
  (define (rev-append xs ys) (foldl cons ys xs))
  (let loop ([sk sk] [peekn peekn] [acc onto] [revsk null])
    (cond [(zero? peekn) (k (rev-append revsk sk) onto acc)]
          [else (with-ststack sk [s1 v2 sk**]
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

(define (glr-parse states vals tz #:mode [mode 'shortest]) ;; mode : (U 'shortest 'all)
  (define DEBUG? #f)
  (define KEEP-FAIL? #t)
  (define-syntax-rule (dprintf fmt arg ...) (when DEBUG? (eprintf fmt arg ...)))
  (define (get-state n) (vector-ref states n))
  (define (get-val n) (if (eq? n 'accept) values (vector-ref vals n)))

  (match-define (tokenizer tz-get-token tz-commit-last) tz)

  (define (get-next-token tr)
    (match tr
      [(? symbol? tk) (tz-get-token tk null)]
      [(cons tf args) (tz-get-token tf args)]))

  (define failed null) ;; mutated; (Listof (U VStack (box VStack))) -- box means #:top failed
  (define ready null) ;; mutated; (Listof StStack); ready to look at next token
  (define done null) ;; mutated; (Listof Result)
  (define collectors '#hash()) ;; mutated; Hash[(cons Symbol Nat) => collect-box]

  ;; run-until-look : StStack Token/#f -> Void
  ;; Runs each "thread" in sk until it needs input and adds to ready
  ;; list. A thread may fork or fail.
  (define (run-until-look sk next-tok)
    (match-define (cons st vsk*) sk)
    (dprintf "\nR2L STATE = #~v, ~s\n" (pstate-index st) (pstate-label st))
    (cond [(pstate-lookahead st)
           => (lambda (lookahead)
                (cond [next-tok
                       (dprintf "-- R2L #~s lookahead (~v)\n"
                                (pstate-index st) (token-name next-tok))
                       (look1 sk next-tok)]
                      [else (push! ready (cons st vsk*))]))]
          [(eq? (pstate-tr st) '#:top)
           (with-vstack vsk* [v1 sk**]
             (cond [(or (hash-ref (pstate-shift st) (token-value* v1 '#:else) #f)
                        (hash-ref (pstate-shift st) '#:else #f))
                    => (lambda (next-state)
                         (run-until-look (list* (get-state next-state) no-value st v1 sk**) next-tok))]
                   [else (fail (box (list* v1 st v1 sk**)) next-tok)]))]
          [else
           (for ([red (pstate-reduce st)] [i (in-naturals)])
             (dprintf "-- R2L #~s reduction ~s/~s\n"
                      (pstate-index st) (add1 i) (length (pstate-reduce st)))
             (reduce sk red next-tok))
           (dprintf "-- R2L #~s continue (~s)\n"
                    (pstate-index st) (and next-tok (token-name next-tok)))
           (cond [next-tok (look1 sk next-tok)]
                 [(not (hash-empty? (pstate-shift st)))
                  (push! ready (cons st vsk*))])]))

  (define (reduce sk red next-tok)
    (match-define (reduction nt index arity ctxn action) red)
    (cond [(eq? action 'accept)
           (with-ststack sk [_st v _sk**]
             ;; If no lookahead, then last token was shifted, so commit.
             (when (not next-tok) (tz-commit-last))
             (push! done v))]
          [else
           (with-ststack-pop/peek-values sk arity ctxn
             (lambda (sk** args all-args)
               (define (mktok v) (make-nt-token nt v args))
               (define value (apply (get-val action) all-args))
               (dprintf "REDUCE(~s): ~v\n" nt value)
               (cond [(action:reject? value)
                      (fail (cons (mktok value) sk**) next-tok)]
                     [(action:collect? value)
                      ;; Could use return state in hash key, but using state
                      ;; after goto should potentially enable more sharing.
                      ;; FIXME: reductions w/ vs w/o lookahead could inhibit sharing.
                      (define next-state (hash-ref (pstate-goto (car sk**)) nt #f))
                      (define cb (collect-value (cons nt next-state) (action:collect-value value)))
                      (when cb (goto (mktok cb) sk** next-tok))]
                     [else (goto (mktok value) sk** next-tok)])))]))

  (define (collect-value prod-id value)
    (cond [(hash-ref collectors prod-id #f)
           => (lambda (cb)
                (let ([h (collect-box-vs cb)])
                  (set-collect-box-vs! cb (hash-set h value #t)))
                ;; If cb exists, another thread has already continued, so we shouldn't.
                #f)]
          [else
           (define cb (collect-box (hash value #t)))
           (set! collectors (hash-set collectors prod-id cb))
           cb]))

  (define (look sks) ;; sks : (Listof StStack)
    (define tr (stacks-consistent-tr sks get-val))
    (define next-tok (get-next-token tr))
    (dprintf "LOOK: read ~v\nSTATES: ~v\n\n"
             next-tok (map pstate-index (map car sks)))
    (for ([sk (in-list sks)]) (look1 sk next-tok)))

  (define (look1 sk next-tok)
    (match-define (cons st vsk*) sk)
    (define reds (hash-ref (or (pstate-lookahead st) #hash()) (token-name next-tok) null))
    (for ([red (in-list reds)] [i (in-naturals)])
      (dprintf "-- L #~s reduction ~s/~s\n" (pstate-index st) (add1 i) (length reds))
      (reduce sk red next-tok))
    (dprintf "-- L #~s continue (~v)\n" (pstate-index st) (token-name next-tok))
    (cond [(hash-ref (pstate-shift st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (run-until-look (list* (get-state next-state) next-tok sk) #f))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (token-name next-tok) #f)
           => (lambda (next-state)
                (dprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (run-until-look (list* (get-state next-state) next-tok sk) #f))]
          [(null? reds)
           (fail (list* next-tok sk) next-tok)]))

  (define (goto reduced sk next-tok)
    (with-ststack sk [st vsk*]
      (dprintf "RETURN VIA #~s\n" (pstate-index st))
      (define next-state (hash-ref (pstate-goto st) (token-name reduced)))
      (dprintf "GOTO ~v\n" next-state)
      (run-until-look (list* (get-state next-state) reduced st vsk*) next-tok)))

  (define (fail vsk* next-tok)
    (when (not next-tok) (tz-commit-last))
    (push! failed vsk*))

  (define (finish-collectors!)
    (for ([cb (in-hash-values collectors)]) (collect-box-finish! cb)))

  (define (run-all-ready)
    (dprintf "\n==== STEP ====\n")
    (define ready* (tjoin-on-cdrs ready))
    (set! ready null)
    (set! failed null)
    (set! collectors '#hash())
    (look ready*))

  (run-until-look (list (get-state 0)) #f)
  (dprintf "\n==== START STEPPING ====\n")
  (let loop ()
    (finish-collectors!)
    (cond [(and (memq mode '(shortest)) (pair? done)) done]
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
     (define (loop vsk acc)
       (if (null? vsk)
           (push! stacks (reverse acc))
           (with-vstack vsk [v st vsk**]
             (loop vsk** (list* (convert-pretty-state st) v acc)))))
     (for ([vsk (in-list (glr-context-vsks self))])
       (loop (if (box? vsk) (unbox vsk) vsk) null))
     stacks)
   (define (context->expected-terminals self)
     (define h (make-hash))
     (define (loop vsk)
       (with-vstack vsk [v1 s2 _]
         (for ([t (in-hash-keys (pstate-shift s2))]) (hash-set! h t #t))))
     (for ([vsk (in-list (glr-context-vsks self))] #:when (not (box? vsk)))
       (loop vsk))
     (if (hash-empty? h) #f (hash-keys h)))
   (define (context->error-lines self)
     (define stacks (context->stacks self))
     (apply string-append
            (format "\n  expected one of: ~s" (context->expected-terminals self))
            (format "\n  got: ~e" (car (car stacks)))
            #;(format "\n  stack count: ~s" (length stacks))
            (let ()
              (define lines null)
              (for ([vsk (in-list (glr-context-vsks self))])
                (with-vstack vsk [_v s _]
                  (push! lines (format "\n  state: ~.s" (convert-pretty-state s)))))
              (remove-duplicates (reverse lines)))))])
