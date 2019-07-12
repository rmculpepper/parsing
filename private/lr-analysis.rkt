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

;; ============================================================

(define START (string->uninterned-symbol "START"))
(define EOF-elem (telem EOF #f))

(define (lr-adjust-grammar g)
  (match-define (grammar start defs vals) g)
  (define start-p (prod START 0 (list (ntelem start) EOF-elem) (vector-length vals)))
  (define vals* (list->vector (append (vector->list vals) (list (lambda (s e) s)))))
  (grammar START (cons (def START (list start-p)) defs) vals))

(define DOT (string->uninterned-symbol "◇"))
(define (DOT? x) (eq? x DOT))

;; An LR0-Item is an Item that contains exactly one DOT symbol.
;; An LR0-Prod is (prod NT Nat LR0-Item Action)

(define (lritem-elem-after-dot lritem)
  (let loop ([lritem lritem])
    (cond [(DOT? (car lritem)) (and (pair? (cdr lritem)) (cadr lritem))]
          [else (loop (cdr lritem))])))

(define (lritem-dot-initial? lritem) (DOT? (car lritem)))
(define (lritem-dot-final? lritem) (DOT? (last lritem)))

(define (prod->initial-lrprod p)
  (match-define (prod nt index item action) p)
  (prod nt index (cons DOT item) action))

(define (lrprod-elem-after-dot lrp)
  (lritem-elem-after-dot (prod-item lrp)))

(define (lrprod-dot-initial? lrp) (lritem-dot-initial? (prod-item lrp)))
(define (lrprod-dot-not-initial? lrp) (not (lrprod-dot-initial? lrp)))
(define (lrprod-dot-final? lrp) (lritem-dot-final? (prod-item lrp)))

(define (lrprod-advance-dot lrp)
  (match-define (prod nt index lritem action) lrp)
  (prod nt index
        (let loop ([lritem lritem])
          (cond [(DOT? (car lritem)) (list* (cadr lritem) DOT (cddr lritem))]
                [else (cons (car lritem) (loop (cdr lritem)))]))
        action))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))
(define (prod->label lrp)
  (match lrp [(prod nt index lritem _) (list* nt index '→ (map elem->label lritem))]))
(define (elem->label elem)
  (match elem
    [(? DOT?) elem]
    [(ntelem nt) nt]
    [(telem t '(default)) t]
    [(telem t #f) t]
    [(telem t (list tr)) (list t tr)]
    [(telem t (list* '#:apply sym index args)) (list* t '#:apply sym args)]
    [(telem t tr) (list t tr)]))

(define debug-consistent #f)

(define (telems-consistent-tr elems [fail #f])
  (define proper-elems (filter telem-tr elems)) ;; ignore polymorphic tokens like EOF
  (match (group-by telem-tr proper-elems)
    [(list) #f]
    [(list group)
     (telem-tr (car group))]
    [groups
     (define kinds (map telem-tr (map car groups)))
     (set! debug-consistent kinds)
     (if fail (fail kinds) (error 'dep-lr "inconsistent token kinds\n  kinds: ~v" kinds))]))

(define-syntax-rule (push! var elem) (set! var (cons elem var)))

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (field [g* (lr-adjust-grammar g)])
    (super-new [g g*])
    (inherit-field start nt-h)
    (inherit nt? nt-follow)

    (define/public (get-vals) (grammar-vals g*))

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt prods) (in-hash nt-h)])
        (values nt (map prod->initial-lrprod prods))))

    (define/public (nt-lrprods nt)
      ;; Returns the list of { NT -> DOT <Item> } productions (that is, DOT-first)
      (hash-ref nt-lrprods-h nt))

    (define/public (lrprod-children lrp)
      (define dotted-elem (lrprod-elem-after-dot lrp))
      (match (lrprod-elem-after-dot lrp)
        [(ntelem nt) (nt-lrprods nt)]
        [_ null]))

    (define state-h (make-hash)) ;; intern table
    (define-syntax-rule (in-states) (in-hash-keys state-h))

    (define/public (state-closure lrps)
      (define state (list-closure lrps (lambda (lrp) (lrprod-children lrp))))
      (hash-ref! state-h state (lambda () state)))

    (define (states-goto-closure state0)
      (define (state-goto-children state)
        (define groups ;; Hash[Symbol => (Listof LR0-Prod)]
          (for/fold ([h (hash)]) ([lrp (in-list state)])
            (cond [(lrprod-elem-after-dot lrp)
                   => (lambda (elem)
                        (hash-set h elem (cons lrp (hash-ref h elem null))))]
                  [else h])))
        (for/hash ([(elem lrps) (in-hash groups)])
          (values elem (state-closure (map lrprod-advance-dot (reverse lrps))))))
      (closure (list state0) state-goto-children #:worklist hash-values))

    (define state0 (state-closure (nt-lrprods start)))
    (define state-goto-h (states-goto-closure state0))

    (define (shift-state? state)
      (let ([goto-h (hash-ref state-goto-h state)])
        (for/or ([sym (in-hash-keys goto-h)]) (telem? sym))))
    (define (reduce-state? state)
      (for/or ([lrp (in-list state)])
        (lrprod-dot-final? lrp)))
    (define (state-goto state sym)
      (hash-ref (hash-ref state-goto-h state) sym #f))

    (define lr0-conflicts
      (append*
       (for/list ([state (in-states)])
         (append
          (if (and (shift-state? state) (reduce-state? state))
              `((shift-reduce ,state)) null)
          (if (< 1 (for/sum ([lrp (in-list state)])
                     (if (lrprod-dot-final? lrp) 1 0)))
              `((reduce-reduce ,state)) null)))))

    ;; ========================================

    (define/private (make-pstates/conflicts)
      (define conflicts null) ;; mutated, (Listof ...)
      (define next-index 0)
      (define (get-index)
        (begin0 next-index (set! next-index (add1 next-index))))
      (define (next-states st) (hash-values (hash-ref state-goto-h st)))
      (define state=>index
        (closure (list state0) next-states #:store (lambda (x) (get-index))))
      (define pstates (make-vector next-index))
      (define (state->pstate st index)
        (define label (lr-state-label st))
        (define shift (for/hash ([(elem st) (in-hash (hash-ref state-goto-h st))]
                                 #:when (telem? elem))
                        (values elem (hash-ref state=>index st))))
        (define goto (for/hash ([(elem st) (in-hash (hash-ref state-goto-h st))]
                                #:when (ntelem? elem))
                       (values elem (hash-ref state=>index st))))
        ;; FIXME: intern shift, goto?
        (define reduce
          (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
            (match-define (prod nt index lritem action) lrp)
            (list nt index (sub1 (length lritem)) action)))
        (define accept
          (cond [(equal? (map car reduce) (list START)) 'true]
                [(equal? (hash-keys shift) (list EOF-elem)) 'virtual]
                [else #f]))
        (define reduce-lookahead (make-reduce-lookahead st index shift reduce))
        (define treader
          (telems-consistent-tr
           (append (hash-keys shift) (if reduce-lookahead (hash-keys reduce-lookahead) null))))
        (define shift* (for/hash ([(elem st) (in-hash shift)]) (values (telem-t elem) st)))
        (define goto* (for/hash ([(elem st) (in-hash goto)]) (values (ntelem-nt elem) st)))
        (define reduce-lookahead*
          (and reduce-lookahead
               (for/hash ([(elem red) (in-hash reduce-lookahead)])
                 (values (telem-t elem) red))))
        (pstate index label treader shift* reduce goto* accept reduce-lookahead*))
      (define (make-reduce-lookahead st index shift reduce)
        (cond [(null? reduce) #f]
              [(and (hash-empty? shift) (<= (length reduce) 1)) #f]
              [else
               (define reduce-lookahead
                 (for/fold ([h (hash)]) ([red (in-list reduce)])
                   (match-define (list red-nt _ _ _) red)
                   (define follows (nt-follow red-nt))
                   (for/fold ([h h]) ([t (in-list follows)])
                     (cond [(hash-ref shift t #f)
                            (begin (push! conflicts (list t 'shift red)) h)]
                           [(hash-ref h t #f)
                            (begin (push! conflicts (list t (hash-ref h t) red)) h)]
                           [else (hash-set h t red)]))))
               (when (pair? conflicts)
                 (void))
               reduce-lookahead]))
      (for ([(st index) (in-hash state=>index)])
        (vector-set! pstates index (state->pstate st index)))
      ;; FIXME: if lookaheads, should be consistent with goto successors (??)
      (values pstates conflicts))

    (define-values (pstates pconflicts) (make-pstates/conflicts))
    (define/public (get-pstates) pstates)
    (define/public (get-pconflicts) pconflicts)

    ;; ========================================

    (define/override (print)
      (super print)
      (when #t
        (printf "LR0 States:\n")
        ;; (pretty-print state-goto-h)
        (pretty-print pstates))
      (when (pair? lr0-conflicts)
        (printf "LR0 Conflicts:\n")
        (pretty-print lr0-conflicts))
      (when (pair? pconflicts)
        (printf "SLR Conflicts:\n")
        (pretty-print pconflicts)))

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
