#lang racket/base
(provide define/memo
         fixpoint
         closure
         list-closure)

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
