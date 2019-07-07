#lang racket/base
(require racket/match
         racket/class
         racket/pretty
         "grammar.rkt"
         "util/stream.rkt")
(provide (all-defined-out))

;; ============================================================

;; UngerItem = ...
(struct unger-nts (nts) #:prefab)
(struct unger-split (left-uitem terminal right-uitem) #:prefab)

;; ============================================================

(define (unger-mixin base%)
  (class base%
    (super-new)
    (inherit-field start defs nt-h)
    (inherit nt? terminal?
             symbol-nullable?
             symbol-minlen)

    ;; ----------------------------------------

    (define unger-nt-h
      (for/hash ([(nt rhss) (in-hash nt-h)])
        (values nt (for/list ([item rhss]) (item->unger-item item)))))

    (define/private (item->unger-item item)
      (define none (unger-nts null))
      (define (init-loop item)
        (cond [(and (pair? item) (terminal? (car item)))
               (unger-split none (car item) (init-loop (cdr item)))]
              [else (final-loop (reverse item))]))
      (define (final-loop item) ;; item is reversed
        (cond [(and (pair? item) (terminal? (car item)))
               (unger-split (final-loop (cdr item)) (car item) none)]
              [#f (left-loop (reverse item) null)]
              [else (right-loop item null)]))
      (define (right-loop item acc) ;; item is reversed
        (cond [(null? item) (unger-nts acc)]
              [(terminal? (car item))
               (unger-split (right-loop (cdr item) null)
                            (car item)
                            (unger-nts acc))]
              [else (right-loop (cdr item) (cons (car item) acc))]))
      (define (left-loop item acc)
        (cond [(null? item) (unger-nts (reverse acc))]
              [(terminal? (car item))
               (unger-split (unger-nts (reverse acc))
                            (car item)
                            (left-loop (cdr item) null))]
              [else (left-loop (cdr item) (cons (car item) acc))]))
      (init-loop item))

    (define/public (unger-parse toks-list)
      (define toks (list->vector toks-list))

      ;; h : Hash[(list NT Nat Nat) => (Streamof Result)]
      (define h (make-hash))

      (define (parse/nt want a b ctx)
        (cond [(memq want ctx) null]
              [(and (= a b) (not (symbol-nullable? want))) null]
              [else (let* ([key (list want a b)] [ectx (cons key ctx)])
                      (hash-ref! h key (lambda () (parse/nt* want a b ectx))))]))
      (define (parse/nt* want a b ectx)
        ;; (eprintf "-- NT ~s from ~a to ~a\n" want a b)
        (let loop ([uitems (hash-ref unger-nt-h want)] [index 0])
          (if (pair? uitems)
              (s-append
               (s-bind (parse/uitem (car uitems) a b ectx)
                       (lambda (result) (s-list (list* want index result))))
               (lambda () (loop (cdr uitems) (add1 index))))
              null)))

      (define (parse/uitem uitem a b ectx)
        (let uloop ([uitem uitem] [a a] [b b])
          (match uitem
            [(unger-nts item)
             (parse/item item a b ectx)]
            [(unger-split (unger-nts '()) terminal right-uitem)
             (cond [(not (< a b)) null]
                   [(eq? (tok-t (vector-ref toks a)) terminal)
                    (s-bind (uloop right-uitem (add1 a) b)
                            (lambda (rresult)
                              (s-list (cons (vector-ref toks a) rresult))))]
                   [else null])]
            [(unger-split left-uitem terminal (unger-nts '()))
             (cond [(not (< a b)) null]
                   [(eq? (tok-t (vector-ref toks (sub1 b))) terminal)
                    (define mid (vector-ref toks (sub1 b)))
                    (s-bind (uloop left-uitem a (sub1 b))
                            (lambda (lresult)
                              (s-list (append lresult (list mid)))))]
                   [else null])]
            #; ;; recur right first
            [(unger-split left-uitem terminal right-uitem)
             (define (join lresult mid rresult)
               (append lresult (cons mid rresult)))
             (let loop ([i a])
               (cond [(not (< i b)) null]
                     [(eq? (tok-t (vector-ref toks i)) terminal)
                      (define mid (vector-ref toks i))
                      (s-append
                       (s-bind (uloop right-uitem (add1 i) b)
                               (lambda (rresult)
                                 (s-bind (uloop left-uitem a i)
                                         (lambda (lresult)
                                           (s-list (join lresult mid rresult))))))
                       (lambda () (loop (add1 i))))]
                     [else (loop (add1 i))]))]
            ;; recur left first
            [(unger-split left-uitem terminal right-uitem)
             (define (join lresult mid rresult)
               (append lresult (cons mid rresult)))
             (let loop ([i a])
               (cond [(not (< i b)) null]
                     [(eq? (tok-t (vector-ref toks i)) terminal)
                      (define mid (vector-ref toks i))
                      (s-append
                       (s-bind (uloop left-uitem a i)
                               (lambda (lresult)
                                 (s-bind (uloop right-uitem (add1 i) b)
                                         (lambda (rresult)
                                           (s-list (join lresult mid rresult))))))
                       (lambda () (loop (add1 i))))]
                     [else (loop (add1 i))]))]
            )))

      (define (parse/item item a b ectx)
        ;; item contains only nts; returns reversed results
        (let loop ([item item] [a a] [acc null])
          ;; (unless (null? item) (eprintf "-- > ~s starting ~s\n" (car item) a))
          (cond [(null? item)
                 (if (= a b) (s-list (reverse acc)) null)]
                [else
                 (define minlen (symbol-minlen (car item)))
                 (s-bind (s-iota (+ a minlen) b)
                         (lambda (a*)
                           (s-bind (parse/nt (car item) a a* ectx)
                                   (lambda (result)
                                     (loop (cdr item) a* (cons result acc))))))])))

      (parse/nt start 0 (vector-length toks) null))

    ;; ----------------------------------------

    (define/override (print)
      (super print)
      (when #t
        (printf "Unger Table:\n")
        (pretty-print unger-nt-h)))
    ))
