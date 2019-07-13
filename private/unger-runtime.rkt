#lang racket/base
(require racket/list
         racket/match
         "grammar-rep.rkt"
         "common.rkt"
         "unger-common.rkt")
(provide (all-defined-out))

;; ============================================================

(define (unger-parse start nts vals toks-list)
  (define toks (list->vector toks-list))

  (define (nt-name nt) (unger-nt-nt (vector-ref nts nt)))
  (define (nt-nullable? nt) (unger-nt-nullable? (vector-ref nts nt)))

  ;; memo-hs : (Vectorof Hash[(cons Nat Nat) => (Listof Result)])
  (define memo-hs (build-vector (vector-length nts) (lambda (i) (make-hash))))
  (define (nt-memo nt) (vector-ref memo-hs nt))

  (define (parse-nt want a b ctx)
    (cond [(memv want ctx) null]
          [(and (= a b) (not (nt-nullable? want))) null]
          [else (hash-ref! (nt-memo want) (cons a b)
                           (lambda () (parse-nt* want a b (cons want ctx))))]))
  (define (parse-nt* want a b ectx)
    (define ntinfo (vector-ref nts want))
    (append*
     (for/list ([p (in-list (unger-nt-prods ntinfo))])
       (parse-prod p a b ectx))))

  (define (parse-prod p a b ectx)
    (match-define (prod nt index uitem action) p)
    (for/list ([result (in-list (parse-uitem uitem a b ectx))])
      (tok nt (apply (vector-ref vals action) result))))

  (define (parse-uitem uitem a0 b0 ectx)
    (let uloop ([uitem uitem] [a a0] [b b0]) ;; ... -> (Listof Result)
      (match uitem
        [(unger-nts item)
         (parse-item item a b a0 b0 ectx)]
        [(unger-split (unger-nts '()) terminal right-uitem)
         (cond [(not (< a b)) null]
               [(eq? (tok-t (vector-ref toks a)) terminal)
                (for/list ([rresult (in-list (uloop right-uitem (add1 a) b))])
                  (cons (vector-ref toks a) rresult))]
               [else null])]
        [(unger-split left-uitem terminal (unger-nts '()))
         (cond [(not (< a b)) null]
               [(eq? (tok-t (vector-ref toks (sub1 b))) terminal)
                (define mid (vector-ref toks (sub1 b)))
                (for/list ([lresult (in-list (uloop left-uitem a (sub1 b)))])
                  (append lresult (list mid)))]
               [else null])]
        [(unger-split left-uitem terminal right-uitem)
         ;; recur left first
         (define (join lresult mid rresult)
           (append lresult (cons mid rresult)))
         (append*
          (for/list ([i (in-range a b)] #:when (eq? (tok-t (vector-ref toks i)) terminal))
            (define mid (vector-ref toks i))
            (for*/list ([lresult (in-list (uloop left-uitem a i))]
                        [rresult (in-list (uloop right-uitem (add1 i) b))])
              (join lresult mid rresult))))]
        )))

  (define (parse-item item a b a0 b0 ectx)
    ;; item contains only nts
    (let loop ([item item] [a a])
      (match item
        ['() (if (= a b) (list null) null)]
        [(cons nt1 item2)
         (for*/list ([a* (in-range a (add1 b))]
                     [result1 (in-list (let ([ctx (if (and (= a a0) (= a* b0)) ectx null)])
                                         (parse-nt nt1 a a* ctx)))]
                     [result2 (in-list (loop item2 a*))])
           (cons result1 result2))])))

  (map cdr (parse-nt start 0 (vector-length toks) null)))
