#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/set
         racket/pretty
         "util.rkt"
         "grammar-rep.rkt"
         "base-analysis.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

;; ============================================================

(define START (string->uninterned-symbol "START"))
(define EOI-elem (telem EOI #f))

(define (lr-adjust-grammar g+)
  (match-define (grammar+ (grammar defs vals) start ends) g+)
  (define start-p (prod START 0 (vector-immutable (ntelem start)) 'accept))
  (grammar+ (grammar (cons (def START 0 (list start-p)) defs) vals) START ends))

(define DOT '•)

;; An LR0-Prod is (lrprod Prod Nat)
;; where dotk is in [0, (vector-length (prod-item p))] --- inclusive!.

(struct lrprod (prod dotk) #:prefab)

(define (prod->initial-lrprod p) (lrprod p 0))
(define (lrprod-dot-initial? lrp) (zero? (lrprod-dotk lrp)))
(define (lrprod-dot-not-initial? lrp) (not (lrprod-dot-initial? lrp)))
(define (lrprod-dot-final? lrp)
  (match lrp [(lrprod (prod _ _ item _) dotk) (= dotk (vector-length item))]))

(define (lrprod-elem-after-dot lrp)
  (match-define (lrprod (prod _ _ item _) dotk) lrp)
  (and (< dotk (vector-length item)) (vector-ref item dotk)))

(define (lrprod-advance-dot lrp)
  (match lrp [(lrprod p dotk) (lrprod p (add1 dotk))]))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))
(define (prod->label lrp)
  (match lrp
    [(lrprod (prod nt index item _) dotk)
     (list* nt index '→ (insert-dot dotk (map elem->label (vector->list item))))]))
(define (elem->label elem)
  (match elem
    [(ntelem nt) nt]
    [(telem t 'default) t]
    [(telem t #f) t]
    [(telem t (? symbol? tr)) (list t tr)]
    [(telem t tr) (list t tr)]
    [(top-elem t) (list* t '#:top)]))
(define (insert-dot k xs)
  (cond [(zero? k) (cons DOT xs)]
        [else (cons (car xs) (insert-dot (sub1 k) (cdr xs)))]))

;; else-elem : Element
;; A top-elem with illegal terminal to encode "else" transitions.
(define else-elem (top-elem '#:else))

;; FIXME: This code assumes that top-elem never occurs first in production. Add
;; check to syntax.rkt, grammar-rep.rkt?

;; FIXME: This code assumes that top-elem does not cause push, is not counted in
;; action routine's arity (or TR's argument indexes).

(define (item->arity elems)
  (for/sum ([elem (in-vector elems)]) (if (top-elem? elem) 0 1)))

;; ------------------------------------------------------------

(define LR%
  (class grammar-base%
    (init-field [lookahead-mode 'slr1]) ;; (U 'lr0 'slr1 'lalr1)
    (super-new)
    (inherit-field g start end def-h)
    (inherit nt? nt-follow nt-ctxn)

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt def) (in-hash def-h)])
        (values nt (map prod->initial-lrprod (def-rhss def)))))

    (define/public (nt-lrprods nt)
      ;; Returns the list of { NT -> DOT <Item> } productions (that is, DOT-first)
      (hash-ref nt-lrprods-h nt))

    (define/public (lrprod-children lrp)
      (match (lrprod-elem-after-dot lrp)
        [(ntelem nt) (nt-lrprods nt)]
        [_ null]))

    ;; ----------------------------------------

    (define state-ix (make-indexer)) ;; Indexer[State]
    (define/private (intern-state state) (indexer-intern! state-ix state))
    (define/private (state-index state) (indexer-get-index state-ix state))
    (define/private (index->state index) (indexer-get-value state-ix index))

    (define/public (kernel->state lrps)
      ;; FIXME: if state generated with kernel (initial productions)
      ;; in different order, then won't get merged with existing?
      (define state (list-closure lrps (lambda (lrp) (lrprod-children lrp))))
      (intern-state state))

    (define state0 (kernel->state (nt-lrprods start)))
    (define-syntax-rule (in-states) (in-indexer-values state-ix))

    (define/private (state-make-next state) ;; State -> Hash[Elem => State]
      (define groups ;; Hash[Elem => (Listof LR0-Prod)]
        (for/fold ([h (hash)]) ([lrp (in-list state)])
          (cond [(lrprod-elem-after-dot lrp)
                 => (lambda (elem) (hash-cons h elem lrp))]
                [else h])))
      (cond [(and (for/or ([elem (in-hash-keys groups)]) (top-elem? elem))
                  (for/or ([elem (in-hash-keys groups)]) (not (top-elem? elem))))
             ;; Both top-elems and t/nt-elems; convert t/nt-first lrps
             (define all-top-elems
               (for/list ([elem (in-hash-keys groups)] #:when (top-elem? elem)) elem))
             (define groups*
               (for/fold ([h (hash)]) ([(elem lrp) (in-hash groups)])
                 (cond [(top-elem? elem)
                        (hash-cons h elem (lrprod-advance-dot lrp))]
                       [else
                        (for/fold ([h h]) ([elem (in-list (cons else-elem all-top-elems))])
                          (hash-cons h elem lrp))])))
             (for/hash ([(elem lrps) (in-hash groups*)])
               (values elem (kernel->state lrps)))]
            [else
             (for/hash ([(elem lrps) (in-hash groups)])
               (values elem (kernel->state (map lrprod-advance-dot (reverse lrps)))))]))

    (define init-states (list (cons state0 start))) ;; (Listof (cons State NT))
    (define state-edge-h ;; Hash[State => Hash[Elem => State]]
      (closure (map car init-states)
               (lambda (state) (state-make-next state))
               #:worklist hash-values))
    (define/public (state-edges state) (hash-ref state-edge-h state))
    (define/public (state-edge state sym) (hash-ref (state-edges state) sym #f))

    ;; state-reduce-origin : State Reduction -> (Listof State)
    (define/public (state-reduce-origin st red)
      ;; Given state and reduction (from a final LR0-Item), returns the states
      ;; that reducing with that rule might *return* to (not the subsequent goto
      ;; state). Note: In the absence of top-elems, if a state contains a
      ;; reduction whose item has N elements, then all predecessor chains must
      ;; have length at least N, and each of the pred^N states originates the
      ;; given production. For top-elems, we distinguish between 0-predecessors
      ;; and 1-predecessors.
      (define (get-0-preds st) (hash-ref (hash-ref state-pred-h st '#hash()) 0 null))
      (define (get-1-preds st) (hash-ref (hash-ref state-pred-h st '#hash()) 1 null))
      (define (loop sts n) ;; (Listof State) Nat -> (Listof State)
        (cond [(zero? n)
               (list-closure sts get-0-preds)]
              [else
               (define sts* (list-closure sts get-0-preds))
               (loop (apply set-union null (map get-1-preds sts*)) (sub1 n))]))
      (loop (list st) (reduction-arity red)))

    (define state-pred-h ;; Hash[State => Hash[(U 0 1) => (Listof State)]]
      (for/fold ([h (hash)]) ([st1 (in-states)])
        (for/fold ([h h]) ([(elem st2) (in-hash (state-edges st1))])
          (hash-update h st2 (lambda (h*) (hash-cons h* (if (top-elem? elem) 0 1) st1)) (hash)))))

    ;; ========================================

    (define/private (state->pstate st index get-lookahead)
      (define label (lr-state-label st))
      (define shift-for-top?
        (for/or ([elem (in-hash-keys (state-edges st))]) (top-elem? elem)))
      (define shift (for/hash ([(elem st) (in-hash (state-edges st))]
                               #:when (t/top-elem? elem))
                      (values (t/top-elem-t elem) (state-index st))))
      (define goto (for/hash ([(elem st) (in-hash (state-edges st))]
                              #:when (ntelem? elem))
                     (values (ntelem-nt elem) (state-index st))))
      ;; FIXME: intern shift, goto?
      (define reduce
        (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
          (match-define (lrprod (prod nt index item action) _) lrp)
          (reduction nt index (item->arity item) (nt-ctxn nt) action)))
      ;; Must check that shift TRs is consistent, also consistent with lookahead
      ;; TRs (if used). NOTE: Cannot use TR w/ args in lookahead! Args depends
      ;; on post-reduction stack indexes AND might refer to value from
      ;; reduction(s).
      (define (consistent-tr elems) (elems-consistent-tr 'lr-parser elems))
      (define-values (tr lookahead/e)
        (cond [shift-for-top?
               ;; => No reductions
               (define tr '#:top)
               (values tr #f)]
              [(null? reduce)
               ;; No reductions => no lookahead needed
               (define tr (consistent-tr (filter telem? (hash-keys (state-edges st)))))
               (values tr #f)]
              [(and (hash-empty? shift) (<= (length reduce) 1))
               ;; No shift, at most one reduction => no lookahead needed
               (values #f #f)]
              [else
               ;; Non-empty shift AND at least one reduction, OR multiple reductions.
               (define lookahead/e (get-lookahead st reduce))
               (define tr
                 (consistent-tr
                  (append (filter telem? (hash-keys (state-edges st)))
                          (hash-keys lookahead/e))))
               (unless (simple-tr? tr)
                 (error 'lr-parser
                        "~a\n  state: ~.s\n  reader: ~.s"
                        "cannot use token reader with arguments for lookahead"
                        label (car tr)))
               (values tr lookahead/e)]))
      (when (eq? end #f) ;; implicit-end mode
        ;; In implicit-end mode, must check that if EOI occurs in reduce lookahead,
        ;; then shift is empty AND no other symbol occurs in reduce lookahead.
        (when (and (hash? lookahead/e) (hash-has-key? lookahead/e EOI-elem))
          (when shift
            (error 'lr-parser "end-of-input conflict with shift\n  shift terminals: ~e"
                   (hash-keys shift)))
          (when (> (hash-count lookahead/e) 1)
            (error 'lr-parser "end-of-input conflict with reduction\n  lookahead: ~e"
                   (for/list ([e (in-hash-keys lookahead/e)] #:when (not (equal? e EOI-elem)))
                     (telem-t e))))))
      (define lookahead
        (and lookahead/e
             (not (eq? lookahead-mode 'lr0))
             (for/hash ([(elem red) (in-hash lookahead/e)])
               (values (telem-t elem) red))))
      (pstate index label tr shift reduce goto lookahead))

    ;; ----------------------------------------

    ;; make-get-lookahead : -> State (Listof Reduction) -> Hash[Elem => (Listof Reduction)]
    (define/private (make-get-lookahead)
      (case lookahead-mode
        ;; We need the lookahead for static analysis reasons, so we compute
        ;; SLR(1) lookahead for 'lr0 too.
        [(lr0 slr1) (make-slr1-lookahead)]
        [(lalr1) (make-lalr1-lookahead)]))

    (define/private ((make-slr1-lookahead) _st reduce)
      (for/fold ([h (hash)]) ([red (in-list reduce)])
        (define red-nt (reduction-nt red))
        (define follows (nt-follow red-nt))
        (for/fold ([h h]) ([t (in-list follows)])
          (hash-cons h t red))))

    (define/private (make-lalr1-lookahead)
      (let* ([g* (reify-lr0)] [gg* (new grammar-base% (g+ g*))])
        (define (follow st nt) (send gg* nt-follow (cons (state-index st) nt)))
        (lambda (st reduce)
          (for/fold ([h (hash)]) ([red (in-list reduce)])
            (define red-la
              (apply set-union null
                     (for/list ([origin-st (in-list (state-reduce-origin st red))])
                       (follow origin-st (reduction-nt red)))))
            (for/fold ([h h]) ([elem (in-set red-la)])
              (hash-cons h elem red))))))

    (define/private (reify-lr0)
      ;; Reifies the LR0 state graph as a grammar.
      (define (mknnt st nt) (cons (state-index st) nt))
      (define (get-nitem st item)
        (for/fold ([st st] [acc null] #:result (list->vector (reverse acc)))
                  ([elem (in-vector item)])
          (define next-st (state-edge st elem))
          (define elem* (match elem [(ntelem nt) (ntelem (mknnt st nt))] [_ elem]))
          (values next-st (cons elem* acc))))
      (define ndef-h ;; FIXME: use lists, group-by instead of hash
        (for*/fold ([h (hash)]) ([st (in-states)] [lrp (in-list st)] #:when (lrprod-dot-initial? lrp))
          (match-define (lrprod (prod nt index item action) 0) lrp)
          (define nnt (mknnt st nt))
          (hash-cons h nnt (prod nnt index (get-nitem st item) 'unused-action))))
      (grammar+ (grammar
                 (for/list ([(nnt prods) (in-hash ndef-h)]) (def nnt 'unused (reverse prods)))
                 'unused-vals)
                (mknnt state0 start) end))

    ;; ----------------------------------------

    (define/private (pstate-conflicts st use-lookahead?)
      (match-define (pstate index _ _ shift reduce _ _) st)
      (define (check-rr reds [t #f])
        (if (> (length reds) 1) (list (conflict:r/r index t)) null))
      (append
       ;; shift/reduce
       (for/list ([t (in-hash-keys shift)]
                  #:when #t
                  [red (in-list
                        (if (and use-lookahead? (pstate-lookahead st))
                            (hash-ref (pstate-lookahead st) t null)
                            (pstate-reduce st)))]
                  [red-i (in-naturals)])
         (conflict:s/r index t))
       ;; reduce/reduce
       (cond [(and use-lookahead? (pstate-lookahead st))
              (append*
               (for/list ([(t reds) (in-hash (pstate-lookahead st))])
                 (check-rr reds t)))]
             [else (check-rr (pstate-reduce st))])))

    ;; ========================================

    (define pstates
      (let ([get-lookahead (make-get-lookahead)])
        (indexer->vector state-ix
                         (lambda (index state)
                           (state->pstate state index get-lookahead)))))

    (define/public (get-pstates) pstates)
    (define/public (get-lr0-conflicts) (get-conflicts #f))
    (define/public (get-conflicts [use-lookahead? #t])
      (append* (for/list ([st (in-vector pstates)]) (pstate-conflicts st use-lookahead?))))

    ;; ========================================

    (define/override (get-summary-data)
      (hash-set* (super get-summary-data)
                 'lookahead-mode lookahead-mode
                 'states (for/vector ([state (indexer->vector state-ix)])
                           (for/list ([lrp (in-list state)])
                             (prod->label lrp)))
                 'pstates pstates
                 'conflicts (get-conflicts)))
    ))

(define (make-LR g+ mode) (new LR% (g+ (lr-adjust-grammar g+)) (lookahead-mode mode)))
