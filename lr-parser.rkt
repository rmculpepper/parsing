#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar.rkt"
         "util/misc.rkt")
(provide (all-defined-out))

;; ============================================================

(define START (string->uninterned-symbol "START"))

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

(define (lprod-advance-dot lrp)
  (match-define (prod nt index lritem) lrp)
  (prod nt index
        (let loop ([lritem lritem])
          (cond [(DOT? (car lritem)) (list* (cadr lritem) DOT (cddr lritem))]
                [else (cons (car lritem) (loop (cdr lritem)))]))))

;; A LR0-State is (Listof LR0-Prod)

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
          (values sym (state-closure (map lprod-advance-dot lrps)))))
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

    (define/public (lr0-parse toks)
      (define (loop toks stack)
        (define state (car stack))
        (eprintf "\nSTATE = ~v\n" state)
        (cond [(shift-state? state)
               ;; FIXME: hacky treatment of EOF, accept state (below)
               (define next-tok (if (pair? toks) (car toks) EOF))
               (define next-t (if (pair? toks) (caar toks) EOF))
               (cond [(state-goto state next-t)
                      => (lambda (next-state)
                           (eprintf "shift ~v\n" next-tok)
                           (loop (if (pair? toks) (cdr toks) toks)
                                 (list* next-state next-tok stack)))]
                     [else (error 'lr0-parse "next = ~v, state = ~v" next-tok state)])]
              [(reduce-state? state)
               ;; FIXME: assumes no conflicts
               (printf "\n*** stack =\n")
               (pretty-print stack)
               (newline)
               (match-define (prod nt index lritem) (car state))
               (define-values (args stack*) (pop-result lritem stack))
               (define value (list* nt index args))
               (eprintf "reduce ~v\n" value)
               (goto toks value stack*)]
              [else (error 'lr0-parse "bad state: ~v" state)]))
      (define (goto toks reduced stack)
        (define state (car stack))
        (eprintf "return to ~v\n" state)
        (eprintf "with stack ~v\n" stack)

        ;; (printf "*** gotos =\n")
        ;; (pretty-print (hash-ref state-goto-h state))

        (cond [(eq? (car reduced) START)
               reduced]
              [else
               (define next-state (state-goto state (car reduced)))
               (eprintf "goto ~v\n" next-state)
               (loop toks (list* next-state reduced stack))]))
      (define (pop-result lritem stack)
        (let loop ([lritem (cdr (reverse lritem))] [stack stack] [acc null])
          (cond [(null? lritem) (values acc stack)]
                [else (loop (cdr lritem) (cddr stack) (cons (cadr stack) acc))])))
      (loop toks (list state0)))

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
