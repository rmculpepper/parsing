#lang racket/base
(require racket/match
         racket/string
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar.rkt"
         "util/misc.rkt")
(provide (all-defined-out))

;; ============================================================

(define START (string->uninterned-symbol "START"))
(define EOF-tok (list EOF))

(define (lr-adjust-grammar g)
  (match-define (grammar start defs) g)
  (grammar START (cons (def START (list (list start EOF))) defs)))

(define DOT (string->uninterned-symbol "◇"))
(define (DOT? x) (eq? x DOT))

;; An LR0-Item is an Item that contains exactly one DOT symbol.
;; An LR0-Prod is (prod NT Nat LR0-Item)
(struct prod (nt index item) #:prefab)

(define (lritem-symbol-after-dot lritem)
  (let loop ([lritem lritem])
    (cond [(DOT? (car lritem)) (and (pair? (cdr lritem)) (cadr lritem))]
          [else (loop (cdr lritem))])))

(define (lrprod-symbol-after-dot lrp)
  (lritem-symbol-after-dot (prod-item lrp)))

(define (lrprod-advance-dot lrp)
  (match-define (prod nt index lritem) lrp)
  (prod nt index
        (let loop ([lritem lritem])
          (cond [(DOT? (car lritem)) (list* (cadr lritem) DOT (cddr lritem))]
                [else (cons (car lritem) (loop (cdr lritem)))]))))

(define (lrprod-dot-not-initial? lrp)
  (match lrp [(prod _ _ lritem) (not (DOT? (car lritem)))]))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (define (prod->label lrp)
    (match lrp [(prod nt index lritem) (list* nt index '→ lritem)]))
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (super-new [g (lr-adjust-grammar g)])
    (inherit-field start nt-h)
    (inherit nt? terminal?)

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt rhss) (in-hash nt-h)])
        (values nt (for/list ([item rhss] [index (in-naturals)])
                     (prod nt index (cons DOT item))))))

    (define/public (nt-lrprods nt)
      ;; Returns the list of { NT -> DOT <Item> } productions (that is, DOT-first)
      (hash-ref nt-lrprods-h nt))

    (define/public (lrprod-children lrp)
      (define dotted-symbol (lrprod-symbol-after-dot lrp))
      (if (nt? dotted-symbol) (nt-lrprods dotted-symbol) null))

    (define state-h (make-hash)) ;; intern table
    (define-syntax-rule (in-states) (in-hash-keys state-h))

    (define/public (state-closure lrps)
      (define state (list-closure lrps (lambda (lrp) (lrprod-children lrp))))
      (hash-ref! state-h state (lambda () state)))

    (define (states-goto-closure state0)
      (define (state-goto-children state)
        (define groups ;; Hash[Symbol => (Listof LR0-Prod)]
          (for/fold ([h (hash)]) ([lrp (in-list state)])
            (cond [(lrprod-symbol-after-dot lrp)
                   => (lambda (dsym)
                        (hash-set h dsym (cons lrp (hash-ref h dsym null))))]
                  [else h])))
        (for/hash ([(sym lrps) (in-hash groups)])
          (values sym (state-closure (map lrprod-advance-dot lrps)))))
      (closure (list state0) state-goto-children #:worklist hash-values))

    (define state0 (state-closure (nt-lrprods start)))
    (define state-goto-h (states-goto-closure state0))

    (define (shift-state? state)
      (let ([goto-h (hash-ref state-goto-h state)])
        (for/or ([sym (in-hash-keys goto-h)]) (terminal? sym))))
    (define (reduce-state? state)
      (match state [(list lrp) (not (lrprod-symbol-after-dot lrp))] [_ #f]))
    (define (state-goto state sym)
      (hash-ref (hash-ref state-goto-h state) sym #f))

    (define lr0-conflicts
      (append*
       (for/list ([state (in-states)])
         (append
          (if (and (shift-state? state) (reduce-state? state))
              `((shift-reduce ,state)) null)
          (if (< 1 (hash-count
                    (for/hash ([lrp state]
                               #:when (not (lrprod-symbol-after-dot lrp)))
                      (values (prod-nt lrp) #t))))
              `((reduce-reduce ,state)) null)))))

    (define/public (naive-lr0-parse toks)
      (define (loop toks stack)
        (define state (car stack))
        ;; (eprintf "\nSTATE = ~v\n" state)
        (cond [(shift-state? state)
               ;; FIXME: hacky treatment of EOF, accept state (below)
               (define next-tok (if (pair? toks) (car toks) EOF))
               (define next-t (if (pair? toks) (caar toks) EOF))
               (cond [(state-goto state next-t)
                      => (lambda (next-state)
                           ;; (eprintf "SHIFT ~v\n" next-tok)
                           (loop (if (pair? toks) (cdr toks) toks)
                                 (list* next-state next-tok stack)))]
                     [else (error 'lr0-parse "next = ~v, state = ~v" next-tok state)])]
              [(reduce-state? state)
               ;; FIXME: assumes no conflicts
               (match-define (prod nt index lritem) (car state))
               (define-values (args stack*) (pop-result lritem stack))
               (define value (list* nt index args))
               ;; (eprintf "REDUCE ~v\n" value)
               (goto toks value stack*)]
              [else (error 'lr0-parse "bad state: ~v" state)]))
      (define (goto toks reduced stack)
        (define state (car stack))
        (cond [(eq? (car reduced) START) reduced]
              [else
               (define next-state (state-goto state (car reduced)))
               ;; (eprintf "GOTO ~v\n" next-state)
               (loop toks (list* next-state reduced stack))]))
      (define (pop-result lritem stack)
        (let loop ([lritem (cdr (reverse lritem))] [stack stack] [acc null])
          (cond [(null? lritem) (values acc stack)]
                [else (loop (cdr lritem) (cddr stack) (cons (cadr stack) acc))])))
      (loop toks (list state0)))

    ;; ========================================

    (define/private (make-pstates)
      (define next-index 0)
      (define (get-index)
        (begin0 next-index (set! next-index (add1 next-index))))
      (define (next-states st) (hash-values (hash-ref state-goto-h st)))
      (define state=>index
        (closure (list state0) next-states #:store (lambda (x) (get-index))))
      (define pstates (make-vector next-index))
      (define (state->pstate st index)
        (define label (lr-state-label st))
        (define shift (for/hash ([(sym st) (in-hash (hash-ref state-goto-h st))]
                                 #:when (terminal? sym))
                        (values sym (hash-ref state=>index st))))
        (define goto (for/hash ([(sym st) (in-hash (hash-ref state-goto-h st))]
                                #:when (nt? sym))
                       (values sym (hash-ref state=>index st))))
        ;; FIXME: intern shift, goto?
        (define reduce
          (for/list ([lrp (in-list st)]
                     #:when (not (lrprod-symbol-after-dot lrp)))
            (match-define (prod nt index lritem) lrp)
            (list nt index (sub1 (length lritem)))))
        (define accept
          (cond [(equal? (map car reduce) (list START)) 'true]
                [(equal? (hash-keys shift) (list EOF)) 'virtual]
                [else #f]))
        (pstate index label shift reduce goto accept))
      (for ([(st index) (in-hash state=>index)])
        (vector-set! pstates index (state->pstate st index)))
      pstates)

    (field [pstates (make-pstates)])
    (define/public (get-pstates) pstates)

    (define/public (lr0-parse toks)
      ;; FIXME: check for conflicts!
      (lr0-parse* pstates toks))

    (define/public (glr-parse toks #:mode [mode 'complete])
      (glr-parse* (make-pstates) toks #:mode mode))

    ;; ========================================

    (define/override (print)
      (super print)
      (when #t
        (printf "LR0 States:\n")
        (pretty-print state-goto-h)
        (when (pair? lr0-conflicts)
          (printf "LR0 Conflicts:\n")
          (pretty-print lr0-conflicts))))
    ))

;; ============================================================

;; PState = (pstate Nat Any PShiftTable PReduce PGotoTable PAccept)
;; PShiftTable = Hash[TerminalSymbol => Nat]
;; PReduce = (Listof (list NT Nat Nat))
;; PGotoTable = Hash[NT => Nat]
;; PAccept = (U #f 'true 'virtual)

;; A "true" accept state is a reduce state
;; - whose LR0-Prod's NT is START, or equivalently (by construction),
;; - whose LR0-Item ends with [EOF DOT].
;; A "virtual" accept state is a shift state
;; - whose only shift edge is EOF.

;; Deterministic LR(0)

(struct pstate (index label shift reduce goto accept) #:prefab)

(define (lr0-parse* states toks)
  (define (loop toks stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "\nSTATE = #~v\n" (car stack))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (cadr (cddr stack))]
                  [(virtual) (cadr stack)]))]
          [(pair? (pstate-reduce st)) ;; (FIXME: assumes no conflicts!)
           (match-define (list (list nt index arity)) (pstate-reduce st))
           (define-values (args stack*) (pop-values arity stack))
           (define value (list* nt index args))
           ;; (eprintf "REDUCE: ~v\n" value)
           (goto toks value stack*)]
          ;; otherwise, shift state (FIXME: assumes no conflicts!)
          [else (shift st toks stack)]))

  (define (shift st toks stack)
    (define next-tok (if (pair? toks) (car toks) EOF-tok))
    (define next-t (tok-t next-tok))
    (cond [(hash-ref (pstate-shift st) next-t #f)
           => (lambda (next-state)
                ;; (eprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (loop (if (pair? toks) (cdr toks) toks)
                      (list* next-state next-tok stack)))]
          [else (error 'lr0-parse "next = ~v, state = ~v" next-tok (car stack))]))

  (define (goto toks reduced stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "RETURN VIA #~s\n" (car stack))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    ;; (eprintf "GOTO ~v\n" next-state)
    (loop toks (list* next-state reduced stack)))
  (loop toks (list 0)))

(define (pop-values arity stack)
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))

;; ============================================================

;; Generalize (Non-Deterministic) LR

(define-syntax-rule (push! var elem) (set! var (cons elem var)))

(define (glr-parse* states toks #:mode [mode 'complete])
  ;; mode : (U 'complete 'first-done 'all)
  (define failed null) ;; mutated; (Listof (cons Tokens Stack))
  (define ready null) ;; mutated; (Listof (cons Tokens Stack))
  (define done null) ;; mutated; (Listof (U Result (cons Result Tokens)))

  ;; run-until-shift: runs and adds to ready
  (define (run-until-shift toks stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "STATE = #~v\n" (car stack))
    (cond [(pstate-accept st)
           (define value
             ;; Did we get here by a shift or a goto?
             (case (pstate-accept st)
               [(true) (cadr (cddr stack))]
               [(virtual) (cadr stack)]))
           (case mode
             [(all first-done) (push! done (cons value toks))]
             [(complete) (cond [(null? toks) (push! done value)]
                               [else (push! failed (cons toks stack))])])]
          [else
           (for ([reduce (pstate-reduce st)])
             (match-define (list nt index arity) reduce)
             (define-values (args stack*) (pop-values arity stack))
             (define value (list* nt index args))
             ;; (eprintf "REDUCE: ~v\n" value)
             (goto toks value stack*))
           (push! ready (cons toks stack))]))

  (define (goto toks reduced stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "RETURN VIA #~s\n" (car stack))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    ;; (eprintf "GOTO ~v\n" next-state)
    (run-until-shift toks (list* next-state reduced stack)))

  ;; shift: does one shift and then runs and adds to ready
  (define (shift toks stack)
    (define st (vector-ref states (car stack)))
    (define next-tok (if (pair? toks) (car toks) EOF-tok))
    (define next-t (tok-t next-tok))
    (cond [(hash-ref (pstate-shift st) next-t #f)
           => (lambda (next-state)
                ;; (eprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (run-until-shift
                 (if (pair? toks) (cdr toks) toks)
                 (list* next-state next-tok stack)))]
          [else (push! failed (cons toks stack))]))

  (define (run-all-shifts)
    ;; (eprintf "\n==== STEP ====\n")
    (define ready* ready)
    (set! ready null)
    (when #t #;only-last-failed? (set! failed null))
    (for ([entry (in-list ready*)])
      ;; (eprintf "\n---- Run ----\n")
      (shift (car entry) (cdr entry))))

  ;; Initialize ready:
  (run-until-shift toks (list 0))
  ;; Loop: Run shifts in lockstep:
  (let loop ()
    (cond [(and (memq mode '(first-done)) (pair? done)) done]
          [(null? ready) done]
          [else (run-all-shifts) (loop)])))
