#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/set
         racket/pretty
         "../util/misc.rkt"
         "grammar-rep.rkt"
         "common.rkt"
         "base-analysis.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

(define (hash-cons h k v) (hash-set h k (cons v (hash-ref h k null))))

;; ============================================================

(define START (string->uninterned-symbol "START"))
(define EOF-elem (telem EOF #f))

(define (lr-adjust-grammar g)
  (match-define (grammar start defs vals) g)
  (define start-p (prod START 0 (vector-immutable (ntelem start) EOF-elem) 'accept))
  (grammar START (cons (def START 0 (list start-p)) defs) vals))

(define DOT (string->uninterned-symbol "•"))
;;(define DOT (string->uninterned-symbol "◇"))

;; An LR0-Prod is (cons Nat Prod), where the nat is in [0, (vector-length item)]
;; and represents the index of the dot in the production's item.

;; An LR0-Item is (cons Nat ElemSequence), where the nat is
;; in [0, (vector-length item)], indicates the position of the dot.

(define (prod->initial-lrprod p) (cons 0 p))
(define (lrprod-prod lrp) (cdr lrp))

(define (lrprod-dot-initial? lrp) (zero? (car lrp)))
(define (lrprod-dot-not-initial? lrp) (not (lrprod-dot-initial? lrp)))
(define (lrprod-dot-final? lrp)
  (match lrp [(cons dotk (prod _ _ item _)) (= dotk (vector-length item))]))

(define (lrprod-elem-after-dot lrp)
  (match lrp
    [(cons dotk (prod _ _ item _))
     (and (< dotk (vector-length item)) (vector-ref item dotk))]))

(define (lrprod-advance-dot lrp)
  (match lrp [(cons dotk p) (cons (add1 dotk) p)]))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))
(define (prod->label lrp)
  (match lrp
    [(cons dotk (prod nt index item _))
     (list* nt index '→ (insert-dot dotk (map elem->label (vector->list item))))]))
(define (elem->label elem)
  (match elem
    [(ntelem nt) nt]
    [(telem t 'default) t]
    [(telem t #f) t]
    [(telem t (? symbol? tr)) (list t tr)]
    [(telem t tr) (list t tr)]
    [(pure-elem t ue) (list* t '#:pure ue)]))
(define (insert-dot k xs)
  (cond [(zero? k) (cons DOT xs)]
        [else (cons (car xs) (insert-dot (sub1 k) (cdr xs)))]))

;; ------------------------------------------------------------

(define LR%
  (class grammar-base%
    (init-field [lookahead-mode 'slr1]) ;; (U 'lr0 'slr1 'lalr1)
    (super-new)
    (inherit-field g start def-h)
    (inherit nt? nt-follow nt-ctxn)

    (define/public (get-vals) (grammar-vals g))

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

    (define state-edge-h ;; Hash[State => Hash[Elem => State]]
      (closure (list state0)
               (lambda (state)
                 (define groups ;; Hash[Elem => (Listof LR0-Prod)]
                   (for/fold ([h (hash)]) ([lrp (in-list state)])
                     (cond [(lrprod-elem-after-dot lrp)
                            => (lambda (elem) (hash-cons h elem lrp))]
                           [else h])))
                 (for/hash ([(elem lrps) (in-hash groups)])
                   (values elem (kernel->state (map lrprod-advance-dot (reverse lrps))))))
               #:worklist hash-values))
    (define/public (state-edges state) (hash-ref state-edge-h state))
    (define/public (state-edge state sym) (hash-ref (state-edges state) sym #f))

    (define state-pred-h ;; Hash[State => Hash[Elem => (Listof State)]]
      (for/fold ([h (hash)]) ([st1 (in-states)])
        (for/fold ([h h]) ([(elem st2) (in-hash (state-edges st1))])
          (hash-update h st2 (lambda (h*) (hash-cons h* elem st1)) (hash)))))
    (define/public (state-all-preds st)
      (apply append (hash-values (hash-ref state-pred-h st))))

    ;; state-reduce-origin : State Reduction -> (Listof State)
    (define/public (state-reduce-origin st red)
      ;; Given state and reduction (from a final LR0-Item), returns the states
      ;; that reducing with that rule might *return* to (not the subsequent goto
      ;; state). Note: if a state contains a reduction whose item has N
      ;; elements, then all predecessor chains must have length at least N, and
      ;; each of the pred^N states originates the given production.
      (for/fold ([sts (list st)]) ([n (in-range (reduction-arity red))])
        (apply set-union null (for/list ([st (in-list sts)]) (state-all-preds st)))))

    ;; ========================================

    (define/private (state->pstate st index get-lookahead)
      (define label (lr-state-label st))
      (define shift (for/hash ([(elem st) (in-hash (state-edges st))]
                               #:when (p/t-elem? elem))
                      (values (p/t-elem-t elem) (state-index st))))
      (define goto (for/hash ([(elem st) (in-hash (state-edges st))]
                              #:when (ntelem? elem))
                     (values (ntelem-nt elem) (state-index st))))
      ;; FIXME: intern shift, goto?
      (define reduce
        (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
          (match-define (cons _ (prod nt index item action)) lrp)
          (reduction nt index (vector-length item) (nt-ctxn nt) action)))
      (define accept
        (cond [(equal? (map reduction-nt reduce) (list start)) 'true]
              [(equal? (hash-keys shift) (list EOF-elem)) 'virtual]
              [else #f]))
      ;; Must check that shift TRs is consistent, also consistent with lookahead
      ;; TRs (if used). NOTE: Cannot use TR w/ args in lookahead! Args depends
      ;; on post-reduction stack indexes AND might refer to value from
      ;; reduction(s).
      (define (consistent-tr elems) (elems-consistent-tr 'lr-parser elems))
      (define-values (tr lookahead/e)
        (cond [(null? reduce)
               ;; No reductions => no lookahead needed
               (define tr (consistent-tr (filter p/t-elem? (hash-keys (state-edges st)))))
               (values tr #f)]
              [(and (hash-empty? shift) (<= (length reduce) 1))
               ;; No shift, at most one reduction => no lookahead needed
               (values #f #f)]
              [else
               ;; Non-empty shift AND at least one reduction, OR multiple reductions.
               (define lookahead/e (get-lookahead st reduce))
               (define tr
                 (consistent-tr
                  (append (filter p/t-elem? (hash-keys (state-edges st)))
                          (hash-keys lookahead/e))))
               (unless (simple-tr? tr)
                 (error 'lr-parser
                        "~a\n  state: ~.s\n  reader: ~.s"
                        "cannot use token reader with arguments for lookahead"
                        label (car tr)))
               (values tr lookahead/e)]))
      (define lookahead
        (and lookahead/e
             (not (eq? lookahead-mode 'lr0))
             (for/hash ([(elem red) (in-hash lookahead/e)])
               (values (p/t-elem-t elem) red))))
      (pstate index label tr shift reduce goto accept lookahead))

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
      (let* ([g* (reify-lr0)] [gg* (new grammar-base% (g g*))])
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
          (match-define (cons 0 (prod nt index item action)) lrp)
          (define nnt (mknnt st nt))
          (hash-cons h nnt (prod nnt index (get-nitem st item) 'unused-action))))
      (grammar (mknnt state0 start)
               (for/list ([(nnt prods) (in-hash ndef-h)]) (def nnt 'unused (reverse prods)))
               'unused-vals))

    ;; ----------------------------------------

    (define/private (pstate-conflicts st use-lookahead?)
      (match-define (pstate index _ _ shift reduce _ _ _) st)
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

    (define/override (print)
      (super print)
      (when #t
        (printf "States:\n")
        ;; (pretty-print state-goto-h)
        (pretty-print pstates))
      (when #t
        (let ([lr0-conflicts (get-lr0-conflicts)])
          (when #t ;; (pair? lr0-conflicts)
            (printf "LR(0) Conflicts: ~v\n" lr0-conflicts))))
      (when #t
        (let ([conflicts (get-conflicts)])
          (when #t ;; (pair? conflicts)
            (printf "~a Conflicts: ~v\n"
                    (case lookahead-mode
                      [(lr0) "LR(0)"] [(slr1) "SLR(1)"] [(lalr1) "LALR(1)"] [else "??"])
                    conflicts)))))

    (define/public (print-states [edges? #t])
      (for ([st pstates]) (print-state st edges?)))
    (define/public (print-state st [edges? #t])
      (match-define (pstate i label tr shift reduce goto accept reduce-lookahead) st)
      (printf "State ~s: ~s\n" i label)
      (when edges?
        (for ([(t next-st) (in-hash shift)])
          (printf "  ~s -> shift ~s\n" t next-st))
        (cond [(hash? reduce-lookahead)
               (for ([(la red) (in-hash reduce-lookahead)])
                 (match-define (list* nt index _) red)
                 (printf "  lookahead ~s -> reduce ~s (~s)\n" la nt index))]
              [else
               (for ([red (in-list reduce)])
                 (match-define (list* nt index _) red)
                 (printf "  -> reduce ~s (~s)\n" nt index))])
        (for ([(nt next-st) (in-hash goto)])
          (printf "  ~s -> goto ~s\n" nt next-st))
        #;(printf "\n")))
    ))

(define (make-LR g mode) (new LR% (g (lr-adjust-grammar g)) (lookahead-mode mode)))
