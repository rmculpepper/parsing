#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "../util/misc.rkt"
         "grammar-rep.rkt"
         "common.rkt"
         "syntax.rkt"
         "base-analysis.rkt")
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

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (define g* (lr-adjust-grammar g))
    (define vals (grammar-vals g*))
    (super-new [g g*])
    (inherit-field start nt-h)
    (inherit nt? nt-follow)

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

    (define/public (parse get-token)
      ;; FIXME: check for conflicts!
      (lr-parse pstates vals get-token))

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

(define-syntax-rule (push! var elem) (set! var (cons elem var)))

;; PState = (pstate Nat Any TReader PShiftTable PReduce PGotoTable PAccept PReduceLookahead)
;; PShiftTable = Hash[TerminalSymbol => Nat]
;; PReduce = (Listof (list NT Nat Nat))
;;   PReduceLookahead = #f | Hash[TerminalSymbol => (list NT Nat Nat)]
;; PGotoTable = Hash[NT => Nat]
;; PAccept = (U #f 'true 'virtual)

;; A "true" accept state is a reduce state
;; - whose LR0-Prod's NT is START, or equivalently (by construction),
;; - whose LR0-Item ends with [EOF DOT].
;; A "virtual" accept state is a shift state
;; - whose only shift edge is EOF.

;; Deterministic LR(0)

(struct pstate (index label tr shift reduce goto accept reduce-lookahead) #:prefab)

;; ============================================================

(define (apply->token f args)
  (define v (apply f args))
  (list (if (token-name? v) v 'bad-token-name)))

(define (token-name? v)
  (or (symbol? v) (exact-integer? v) (boolean? v) (char? v)))

(define (lr-parse states vals tz)
  (define DEBUG? #f)
  (define (get-token peek? tr stack)
    (cond [(symbol? (car tr))
           (tz peek? (car tr) (get-token-args (cdr tr) stack))]
          [(eq? (car tr) '#:apply)
           (apply->token (vector-ref vals (caddr tr)) (get-token-args (cdddr tr) stack))]
          [else (error 'lr-parse "bad tr: ~e" tr)]))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        [(list datum) datum]
        [(? exact-nonnegative-integer? index)
         (tok-v (list-ref stack (+ index index -1)))])))

  (define (loop stack)
    (define st (vector-ref states (car stack)))
    (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (car stack) (pstate-label st)))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (tok-v (cadr (cddr stack)))]
                  [(virtual) (tok-v (cadr stack))]))]
          [(pstate-reduce-lookahead st)
           => (lambda (reduce-lookahead)
                (define next-tok (get-token #t (pstate-tr st) stack))
                (cond [(hash-ref reduce-lookahead (tok-t next-tok) #f)
                       => (lambda (red) (reduce st stack red))]
                      [else (shift st stack)]))]
          [(pair? (pstate-reduce st)) ;; (FIXME: assumes no conflicts!)
           (reduce st stack (car (pstate-reduce st)))]
          ;; otherwise, shift state (FIXME: assumes no conflicts!)
          [else (shift st stack)]))

  (define (reduce st stack red)
    (match-define (list nt index arity action) red)
    (define-values (args stack*) (pop-values arity stack))
    (define value (tok nt (apply (vector-ref vals action) args))) ;; (list* nt index args)
    (when DEBUG? (eprintf "REDUCE: ~v\n" value))
    (goto value stack*))

  (define (shift st stack)
    (define next-tok (get-token #f (pstate-tr st) stack))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (loop (list* next-state next-tok stack)))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (loop (list* next-state next-tok stack)))]
          [else (error 'lr-parse "next = ~v, state = ~v" next-tok (car stack))]))

  (define (goto reduced stack)
    (define st (vector-ref states (car stack)))
    (when DEBUG? (eprintf "RETURN VIA #~s\n" (car stack)))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    (when DEBUG? (eprintf "GOTO ~v\n" next-state))
    (loop (list* next-state reduced stack)))
  (loop (list 0)))

(define (pop-values arity stack) ;; produces values in original order
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))
