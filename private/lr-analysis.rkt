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
  (define start-p (prod START 0 (vector-immutable (ntelem start) EOF-elem) (vector-length vals)))
  (define vals* (list->vector (append (vector->list vals) (list (lambda (s e) s)))))
  (grammar START (cons (def START (list start-p)) defs) vals))

(define DOT (string->uninterned-symbol "•"))
;;(define DOT (string->uninterned-symbol "◇"))

;; An LR0-Prod is (cons Nat Prod), where the nat is in [0, (vector-length item)]
;; and represents the index of the dot in the production's item.

;; An LR0-Item is (cons Nat ElemSequence), where the nat is
;; in [0, (vector-length item)], indicates the position of the dot.

;; An LR0-Prod is (prod NT Nat LR0-Item Action)

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
    [(telem t '(default)) t]
    [(telem t #f) t]
    [(telem t (list tr)) (list t tr)]
    [(telem t (list* '#:apply sym index args)) (list* t '#:apply sym args)]
    [(telem t tr) (list t tr)]))
(define (insert-dot k xs)
  (cond [(zero? k) (cons DOT xs)]
        [else (cons (car xs) (insert-dot (sub1 k) (cdr xs)))]))

;; ------------------------------------------------------------

(define LR%
  (class grammar-base%
    (init-field [lookahead-mode 'slr1]) ;; (U 'lr0 'slr1 'lalr1)
    (super-new)
    (inherit-field g start nt-h)
    (inherit nt? nt-follow)

    (define/public (get-vals) (grammar-vals g))

    (define-syntax-rule (push! var elem) (set! var (cons elem var)))

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt prods) (in-hash nt-h)])
        (values nt (map prod->initial-lrprod prods))))

    (define/public (nt-lrprods nt)
      ;; Returns the list of { NT -> DOT <Item> } productions (that is, DOT-first)
      (hash-ref nt-lrprods-h nt))

    (define/public (lrprod-children lrp)
      (match (lrprod-elem-after-dot lrp)
        [(ntelem nt) (nt-lrprods nt)]
        [_ null]))

    (define state-intern-h (make-hash)) ;; intern table
    (define/private (intern-state state) (hash-ref! state-intern-h state state))

    (define/public (kernel->state lrps)
      (define state (list-closure lrps (lambda (lrp) (lrprod-children lrp))))
      (intern-state state))

    (define state0 (kernel->state (nt-lrprods start)))
    (define-syntax-rule (in-states) (in-hash-keys state-intern-h))

    (define state-edge-h
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

    (define state-pred-h
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

    ;; state-reduce-goto : State Reduction -> (Listof State)
    (define/public (state-reduce-goto st red)
      ;; Given state and reduction (from a final LR0-Item), returns the states
      ;; that might follow from a single reduce/goto pair of edges.
      (apply set-union null
             (for/list ([ret-st (in-list (state-reduce-origin st red))])
               (state-edge ret-st (reduction-nt red)))))

    ;; ========================================

    (define state-index-h
      (let ([next-index 0])
        (define (get-index) (begin0 next-index (set! next-index (add1 next-index))))
        (closure (list state0)
                 (lambda (state) (hash-values (state-edges state)))
                 #:store (lambda (x) (get-index)))))
    (define/private (state-index state) (hash-ref state-index-h state))
    (define/private (state-count) (hash-count state-index-h))

    (define state-from-index-h
      (for/fold ([h (hash)]) ([(s i) (in-hash state-index-h)]) (hash-set h i s)))
    (define/private (index->state index) (hash-ref state-from-index-h index))

    ;; ========================================

    (define/public (reify-lr0)
      ;; Reifies the LR0 state graph as a grammar.
      (define nnt-intern-h (make-hash))
      (define (mknnt st nt) (cons (state-index st) nt))
      (define (get-nitem st item)
        (list->vector
         (let loop ([st st] [i 0])
           (cond [(< i (vector-length item))
                  (define elemi (vector-ref item i))
                  (define next-st (state-edge st elemi))
                  (cons (match elemi [(ntelem nt) (ntelem (mknnt st nt))] [_ elemi])
                        (loop next-st (add1 i)))]
                 [else null]))))
      (define ndef-h ;; FIXME: use lists, group-by instead of hash
        (for*/fold ([h (hash)]) ([st (in-states)] [lrp (in-list st)] #:when (lrprod-dot-initial? lrp))
          (match-define (cons 0 (prod nt index item action)) lrp)
          (define nnt (mknnt st nt))
          (hash-cons h nnt (prod nnt index (get-nitem st item) action))))
      (grammar (mknnt state0 start)
               (for/list ([(nnt prods) (in-hash ndef-h)]) (def nnt (reverse prods)))
               (get-vals)))

    ;; ========================================

    (define/public (make-pstates)
      (define make-lookahead
        (case lookahead-mode
          [(lr0) (lambda (st reduce) #f)]
          [(slr1) (lambda (st reduce) (slr1-lookahead reduce))]
          [(lalr1) (let* ([g* (reify-lr0)] [gg* (new grammar-base% (g g*))])
                     (define (follow st nt) (send gg* nt-follow (cons (state-index st) nt)))
                     (lambda (st reduce) (lalr1-lookahead follow st reduce)))]))
      (define (state->pstate st index)
        (define label (lr-state-label st))
        (define shift (for/hash ([(elem st) (in-hash (state-edges st))] #:when (telem? elem))
                        (values (telem-t elem) (state-index st))))
        (define goto (for/hash ([(elem st) (in-hash (state-edges st))] #:when (ntelem? elem))
                       (values (ntelem-nt elem) (state-index st))))
        ;; FIXME: intern shift, goto?
        (define reduce
          (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
            (match-define (cons _ (prod nt index item action)) lrp)
            (reduction nt index (vector-length item) action)))
        (define accept
          (cond [(equal? (map reduction-nt reduce) (list start)) 'true]
                [(equal? (hash-keys shift) (list EOF-elem)) 'virtual]
                [else #f]))
        (define lookahead/e
          (cond [(null? reduce) #f]
                [(and (hash-empty? shift) (<= (length reduce) 1)) #f]
                [else (make-lookahead st reduce)]))
        (define treader
          (telems-consistent-tr
           'lr-parser
           (append (filter telem? (hash-keys (state-edges st)))
                   (if lookahead/e (hash-keys lookahead/e) null))))
        (define lookahead
          (and lookahead/e
               (for/hash ([(elem red) (in-hash lookahead/e)])
                 (values (telem-t elem) red))))
        (pstate index label treader shift reduce goto accept lookahead))
      (define pstates (make-vector (state-count)))
      (for ([(st index) (in-hash state-index-h)])
        (vector-set! pstates index (state->pstate st index)))
      ;; FIXME: if lookaheads, should be consistent with goto successors (??)
      pstates)

    (define/public (pstate-lr0-conflicts st)
      (match-define (pstate index _ _ shift reduce _ _ _) st)
      (cond [(null? reduce) null]
            [(and (hash-empty? shift) (<= (length reduce) 1)) null]
            [(hash-empty? shift) (list (conflict index #f #f reduce))]
            [else (list (conflict index #f #t reduce))]))

    (define/public (pstate-conflicts st)
      (match-define (pstate index _ _ shift _ _ _ lookahead) st)
      (filter values
              (for/list ([(t reds) (in-hash (or lookahead (hash)))])
                (define shift-st (hash-ref shift t #f))
                (and (> (+ (length reds) (if shift-st 1 0)) 1)
                     (conflict index t shift-st reds)))))

    (define/public (slr1-lookahead reduce) ;; -> Hash[Elem => (Listof Reduction)]
      (for/fold ([h (hash)]) ([red (in-list reduce)])
        (match-define (reduction red-nt _ _ _) red)
        (define follows (nt-follow red-nt))
        (for/fold ([h h]) ([t (in-list follows)])
          (hash-cons h t red))))

    (define/public (lalr1-lookahead follow st reduce)
      (for/fold ([h (hash)]) ([red (in-list reduce)])
        (define red-la
          (apply set-union null
                 (for/list ([origin-st (in-list (state-reduce-origin st red))])
                   (follow origin-st (reduction-nt red)))))
        (for/fold ([h h]) ([elem (in-set red-la)])
          (hash-cons h elem red))))

    (define pstates (make-pstates))
    (define/public (get-pstates) pstates)
    (define/public (get-lr0-conflicts)
      (append* (for/list ([st (in-vector pstates)]) (pstate-lr0-conflicts st))))
    (define/public (get-conflicts)
      (append* (for/list ([st (in-vector pstates)]) (pstate-conflicts st))))

    ;; ========================================

    (define/override (print)
      (super print)
      (when #t
        (printf "LR0 States:\n")
        ;; (pretty-print state-goto-h)
        (pretty-print pstates))
      (let ([lr0-conflicts (get-lr0-conflicts)])
        (when (pair? lr0-conflicts)
          (printf "LR0 Conflicts:\n")
          (pretty-print lr0-conflicts)))
      (let ([conflicts (get-conflicts)])
        (when (pair? conflicts)
          (printf "SLR Conflicts:\n")
          (pretty-print conflicts))))

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
