#lang racket/base
(require racket/match
         racket/class
         racket/list
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

(define DOT (string->uninterned-symbol "◇"))

;; An LR0-Prod is (cons Nat Prod), where the nat is in [0, (vector-length item)]
;; and represents the index of the dot in the production's item.

;; An LR0-Item is (cons Nat ElemSequence), where the nat is
;; in [0, (vector-length item)], indicates the position of the dot.

;; An LR0-Prod is (prod NT Nat LR0-Item Action)

(define (prod->initial-lrprod p) (cons 0 p))

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

(define (telems-consistent-tr elems [fail #f])
  (define proper-elems (filter telem-tr elems)) ;; ignore polymorphic tokens like EOF
  (match (group-by telem-tr proper-elems)
    [(list) '(default)]
    [(list group)
     (telem-tr (car group))]
    [groups
     (define kinds (map telem-tr (map car groups)))
     (if fail (fail kinds) (error 'dep-lr "inconsistent token kinds\n  kinds: ~v" kinds))]))

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (field [g* (lr-adjust-grammar g)])
    (super-new [g g*])
    (inherit-field start nt-h)
    (inherit nt? nt-follow)

    (define/public (get-vals) (grammar-vals g*))

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

    ;; ========================================

    (define state-index-h
      (let ([next-index 0])
        (define (get-index) (begin0 next-index (set! next-index (add1 next-index))))
        (closure (list state0)
                 (lambda (state) (hash-values (state-edges state)))
                 #:store (lambda (x) (get-index)))))
    (define/private (state-index state) (hash-ref state-index-h state))
    (define/private (state-count) (hash-count state-index-h))

    ;; ========================================

    (define/private (make-pstates)
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
          (cond [(equal? (map reduction-nt reduce) (list START)) 'true]
                [(equal? (hash-keys shift) (list EOF-elem)) 'virtual]
                [else #f]))
        (define lookahead/e
          (cond [(null? reduce) #f]
                [(and (hash-empty? shift) (<= (length reduce) 1)) #f]
                [else (slr-lookahead reduce)]))
        (define treader
          (telems-consistent-tr
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

    (define/public (slr-lookahead reduce) ;; -> Hash[Elem => (Listof Reduction)]
      (for/fold ([h (hash)]) ([red (in-list reduce)])
        (match-define (reduction red-nt _ _ _) red)
        (define follows (nt-follow red-nt))
        (for/fold ([h h]) ([t (in-list follows)])
          (hash-cons h t red))))

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

(define LR% (LR-mixin grammar-base%))
(define (make-LR g) (new LR% (g g)))
