#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "util/misc.rkt")
(provide (all-defined-out))

;; ============================================================

;; A Grammar is (grammar NT (Listof Def))
(struct grammar (start defs) #:prefab)

;; A Def is (def NT (Listof Item))
;; An Item is (Listof GSymbol)
(struct def (nt rhss) #:prefab)

;; NT = NonterminalSymbol, T = TerminalSymbol
;; GSymbol = (U NonterminalSymbol TerminalSymbol)

(define-syntax-rule (Grammar start [nt (sym ...) ...] ...)
  (grammar 'start (list (def 'nt `((sym ...) ...)) ...)))

;; ============================================================

;; A Token is (cons Symbol Any); the symbol is the Terminal name.
(define (tok-t tok) (car tok))

(define EOF (string->uninterned-symbol "EOF"))

;; ============================================================

(define grammar-base%
  (class object%
    (init g)
    (field [start (grammar-start g)]
           [defs (grammar-defs g)])
    (super-new)

    ;; ----------------------------------------

    (define/public (generate-corpus n)
      (for/list ([i (in-range n)]) (generate start)))

    (define/public (generate [want start])
      (cond [(nt? want)
             (define rhss (nt-rhss want))
             (append*
              (for/list ([sym (list-ref rhss (random (length rhss)))])
                (generate sym)))]
            [else `((,want))]))

    ;; ----------------------------------------

    (field [nt-h (for/hash ([def defs]) (values (def-nt def) (def-rhss def)))])

    (define/public (nt? sym) (and (hash-ref nt-h sym #f) #t))
    (define/public (terminal? sym) (and (symbol? sym) (not (nt? sym))))

    (define/public (nt-rhss nt)
      (or (hash-ref nt-h nt #f) (error 'nt-rhss "undefined nonterminal: ~e" nt)))

    ;; ----------------------------------------

    (define nt-nullable-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def defs])
           (hash-set h (def-nt def)
                     (for/or ([item (def-rhss def)]) (item-nullable? item #:h h)))))
       (hash)))

    (define/public (symbol-nullable? sym #:h [h nt-nullable-h])
      (if (nt? sym) (hash-ref h sym #t) #f))
    (define/public (item-nullable? item #:h [h nt-nullable-h])
      (for/and ([sym item]) (symbol-nullable? sym #:h h)))

    ;; ----------------------------------------

    (define MAX-MINLEN 20)
    (define nt-minlen-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def defs])
           (hash-set h (def-nt def)
                     (apply min MAX-MINLEN
                            (for/list ([item (def-rhss def)])
                              (item-minlen item #:h h))))))
       (hash)))

    (define/public (symbol-minlen sym #:h [h nt-minlen-h])
      (if (nt? sym) (hash-ref h sym 0) 1))
    (define/public (item-minlen item #:h [h nt-minlen-h])
      (for/sum ([sym item]) (symbol-minlen sym #:h h)))

    ;; ----------------------------------------

    (define nt-first-h
      (fixpoint
       (lambda (h)
         (for/hash ([def defs])
           (values (def-nt def)
                   (apply set-union '()
                          (for/list ([item (def-rhss def)])
                            (item-first item #:h h))))))
       (hash)))

    (define/public (symbol-first sym #:h [h nt-first-h])
      (if (nt? sym) (hash-ref h sym null) (list sym)))
    (define/public (item-first item #:h [h nt-first-h])
      (let loop ([item item])
        (match item
          [(cons sym1 item2)
           (if (symbol-nullable? sym1)
               (set-union (symbol-first sym1 #:h h) (loop item2))
               (symbol-first sym1 #:h h))]
          ['() null])))

    ;; ----------------------------------------

    (define nt-follow-h
      (fixpoint
       (lambda (h)
         (for*/fold ([h h]) ([def defs] [item (def-rhss def)])
           (for/fold ([h h] [follows-sym (hash-ref h (def-nt def) null)]
                      #:result h)
                     ([sym (reverse item)])
             (if (nt? sym)
                 (values (hash-set h sym (set-union (hash-ref h sym null) follows-sym))
                         (if (symbol-nullable? sym)
                             (set-union follows-sym (symbol-first sym))
                             (symbol-first sym)))
                 (values h (list sym))))))
       (hash start (list EOF))))

    (define/public (nt-follow nt #:h [h nt-follow-h])
      (hash-ref h nt null))

    ;; ----------------------------------------

    (define/public (print)
      (printf "Nullable:\n")
      (pretty-print nt-nullable-h)
      (printf "Min length:\n")
      (pretty-print nt-minlen-h)
      (printf "First:\n")
      (pretty-print nt-first-h)
      (printf "Follow:\n")
      (pretty-print nt-follow-h))
    ))
