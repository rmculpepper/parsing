#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/set
         "../util/misc.rkt"
         "grammar-rep.rkt")
(provide (all-defined-out))

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
      (define rhss (nt-rhss want))
      (append*
       (for/list ([elem (in-vector (prod-item (list-ref rhss (random (length rhss)))))])
         (match elem
           [(ntelem nt) (generate nt)]
           [(telem t _) (list (list t))]))))

    ;; ----------------------------------------

    (field [nt-h (for/hash ([def defs]) (values (def-nt def) (def-rhss def)))])

    (define/public (nt? sym) (and (hash-ref nt-h sym #f) #t))

    (define/public (nt-rhss nt)
      (or (hash-ref nt-h nt #f) (error 'nt-rhss "undefined nonterminal: ~e" nt)))

    ;; ----------------------------------------

    (define nt-nullable-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def (in-list defs)])
           (hash-set h (def-nt def)
                     (for/or ([p (in-list (def-rhss def))])
                       (item-nullable? (prod-item p) #:h h)))))
       (hash)))

    ;; *-nullable? : ... -> Boolean
    (define/public (nt-nullable? sym #:h [h nt-nullable-h])
      (hash-ref h sym #t))
    (define/public (elem-nullable? elem #:h [h nt-nullable-h])
      (match elem [(ntelem nt) (nt-nullable? nt #:h h)] [_ #f]))
    (define/public (item-nullable? item #:h [h nt-nullable-h])
      (for/and ([elem (in-vector item)]) (elem-nullable? elem #:h h)))

    ;; ----------------------------------------

    (define MAX-MINLEN 20)
    (define nt-minlen-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def (in-list defs)])
           (hash-set h (def-nt def)
                     (apply min MAX-MINLEN
                            (for/list ([p (def-rhss def)])
                              (item-minlen (prod-item p) #:h h))))))
       (hash)))

    ;; *-minlen : ... -> Nat
    (define/public (nt-minlen nt #:h [h nt-minlen-h])
      (hash-ref h nt 0))
    (define/public (elem-minlen elem #:h [h nt-minlen-h])
      (match elem [(ntelem nt) (nt-minlen nt #:h h)] [_ 1]))
    (define/public (item-minlen item #:h [h nt-minlen-h])
      (for/sum ([elem (in-vector item)]) (elem-minlen elem #:h h)))

    ;; ----------------------------------------

    (define nt-first-h
      (fixpoint
       (lambda (h)
         (for/hash ([def defs])
           (values (def-nt def)
                   (apply set-union (hash-ref h (def-nt def) '())
                          (for/list ([p (in-list (def-rhss def))])
                            (item-first (prod-item p) #:h h))))))
       (hash)))

    ;; *-first : ... -> (Listof telem)
    (define/public (nt-first nt #:h [h nt-first-h])
      (hash-ref h nt null))
    (define/public (elem-first elem #:h [h nt-first-h])
      (match elem [(ntelem nt) (nt-first nt #:h h)] [t (list t)]))
    (define/public (item-first item #:h [h nt-first-h])
      (let loop ([i 0])
        (cond [(< i (vector-length item))
               (define elemi (vector-ref item i))
               (set-union (elem-first elemi #:h h)
                          (if (elem-nullable? elemi) (loop (add1 i)) null))]
              [else null])))

    ;; ----------------------------------------

    (define nt-final-h
      (fixpoint
       (lambda (h)
         (for/hash ([def defs])
           (values (def-nt def)
                   (apply set-union (hash-ref h (def-nt def) '())
                          (for/list ([p (in-list (def-rhss def))])
                            (item-final (prod-item p) #:h h))))))
       (hash)))

    ;; *-final : ... -> (Listof telem)
    (define/public (nt-final nt #:h [h nt-final-h])
      (hash-ref h nt null))
    (define/public (elem-final elem #:h [h nt-final-h])
      (match elem [(ntelem nt) (nt-final nt #:h h)] [t (list t)]))
    (define/public (item-final item #:h [h nt-final-h])
      (let loop ([i (sub1 (vector-length item))])
        (cond [(>= i 0)
               (define elemi (vector-ref item i))
               (set-union (elem-final elemi #:h h)
                          (if (elem-nullable? elemi) (loop (sub1 i)) null))]
              [else null])))

    ;; ----------------------------------------

    (define nt-follow-h
      (fixpoint
       (lambda (h)
         (for*/fold ([h h]) ([def (in-list defs)] [p (in-list (def-rhss def))])
           (define item (prod-item p))
           (for/fold ([h h] [follows-this (hash-ref h (def-nt def) null)] #:result h)
                     ([elem (in-list (reverse (vector->list item)))])
             (match elem
               [(ntelem nt)
                (values (hash-set h nt (set-union (hash-ref h nt null) follows-this))
                        (set-union (nt-first nt)
                                   (if (nt-nullable? nt) follows-this null)))]
               [_ (values h (list elem))]))))
       (hash start (list EOF))))

    ;; *-follow : ... -> (Listof telem)
    (define/public (nt-follow nt #:h [h nt-follow-h])
      (hash-ref h nt null))

    ;; ----------------------------------------

    (define/public (print)
      (printf "Nullable: ~s\n"
              (for/list ([(nt n?) (in-hash nt-nullable-h)] #:when n?) nt))
      (printf "Min length: ~s\n"
              (for/list ([(nt len) (in-hash nt-minlen-h)]) (list nt len)))
      (printf "First: ~s\n"
              (for/list ([(nt fs) (in-hash nt-first-h)])
                (list nt ': (map telem-t fs))))
      (printf "Follow: ~s\n"
              (for/list ([(nt fs) (in-hash nt-follow-h)])
                (list nt ': (for/list ([f fs]) (if (telem? f) (telem-t f) f))))))
    ))
