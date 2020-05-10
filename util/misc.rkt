#lang racket/base
(provide (all-defined-out))

(define-syntax-rule (define/memo (fun arg ...) . body)
  (begin (define fun-table (make-hash))
         (define (fun arg ...)
           (hash-ref! fun-table (memo-key arg ...) (lambda () . body)))))

(define memo-key (case-lambda [(arg) arg] [args args]))

(define (fixpoint refine . init-vals)
  (let loop ([init-vals init-vals])
    (define new-vals (call-with-values (lambda () (apply refine init-vals)) list))
    (if (equal? new-vals init-vals) (apply values new-vals) (loop new-vals))))

(define (closure worklist get-children
                 #:store [store-fun values]
                 #:worklist [worklist-fun values])
  (let loop ([h (hash)] [worklist worklist])
    (cond [(null? worklist) h]
          [(hash-ref h (car worklist) #f)
           (loop h (cdr worklist))]
          [else
           (define children (get-children (car worklist)))
           (loop (hash-set h (car worklist) (store-fun children))
                 (append (worklist-fun children) (cdr worklist)))])))

(define (list-closure worklist get-children)
  (let loop ([acc null] [worklist worklist] [h (hash)])
    (cond [(null? worklist) (reverse acc)]
          [(hash-ref h (car worklist) #f) (loop acc (cdr worklist) h)]
          [else
           (define children (get-children (car worklist)))
           (loop (cons (car worklist) acc)
                 (append children (cdr worklist))
                 (hash-set h (car worklist) #t))])))

;; ----------------------------------------

;; Indexer[X] = (indexer Hash[X => Nat] Hash[Nat => X])
(struct indexer (to from) #:transparent)

(define (make-indexer [seq null])
  (define ix (indexer (make-hash) (make-hasheqv)))
  (for ([e seq]) (indexer-add! e))
  ix)

(define (indexer-count ix) (hash-count (indexer-to ix)))

(define (indexer-add! ix e)
  (define to (indexer-to ix))
  (cond [(hash-ref to e #f)
         => values]
        [else
         (define from (indexer-from ix))
         (define k (hash-count to))
         (hash-set! to e k)
         (hash-set! from k e)
         k]))

(define (indexer-intern! ix e)
  (define k (indexer-add! ix e))
  (hash-ref (indexer-from ix) k))

(define (indexer-get-index ix e)
  (hash-ref (indexer-to ix) e #f))
(define (indexer-get-value ix k)
  (hash-ref (indexer-from ix) k #f))

(define (indexer->vector ix [f (lambda (k v) v)])
  (define from (indexer-from ix))
  (define vec (make-vector (hash-count from)))
  (for ([(k v) (in-hash from)]) (vector-set! vec k (f k v)))
  vec)

(define-syntax-rule (in-indexer ix)
  (in-hash (indexer-to ix)))

(define-syntax-rule (in-indexer-values ix)
  ;; Note: not ordered!
  (in-hash-keys (indexer-to ix)))
