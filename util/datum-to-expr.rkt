#lang racket/base
(require (for-template racket/base))
(provide datum->expression)

;; datum->expression : Datum -> Syntax[Expr]
;; Preserves eq-sharing of syntax.
(define (datum->expression v [convert (lambda (v) #f)])
  (define syntax-h (make-hash))
  (define (make-syntax-ref stx)
    (hash-ref! syntax-h stx (lambda () (car (generate-temporaries '(stx))))))
  (define (const v) `(quote ,v))
  (define (const? e) (and (pair? e) (eq? (car e) 'quote)))
  (define (loop v)
    (cond [(convert v) => values]
          [(syntax? v)
           (make-syntax-ref `(quote-syntax ,v))]
          [(pair? v)
           (cond [(and (list? v) (andmap syntax? v))
                  `(syntax->list (quote-syntax ,(datum->syntax #f v)))]
                 [else
                  (define outer-v v)
                  (let pairloop ([v v] [acc null])
                    (cond [(pair? v)
                           (pairloop (cdr v) (cons (loop (car v)) acc))]
                          [(null? v)
                           (cond [(andmap const? acc) (const outer-v)]
                                 [else `(list ,@(reverse acc))])]
                          [else
                           (let ([acc (cons (loop v) acc)])
                             (cond [(andmap const? acc) (const outer-v)]
                                   [else `(list* ,@(reverse acc))]))]))])]
          [(vector? v)
           (let ([elem-es (map loop (vector->list v))])
             (cond [(andmap const? elem-es) (const v)]
                   [else `(vector-immutable ,@elem-es)]))]
          [(prefab-struct-key v)
           => (lambda (key)
                (define elem-es (map loop (cdr (vector->list (struct->vector v)))))
                (cond [(andmap const? elem-es) (const v)]
                      [else `(make-prefab-struct (quote ,key) ,@elem-es)]))]
          ;; FIXME: boxes, hashes?
          [else
           (const v)]))
  (let ([main-expr (loop v)])
    (datum->syntax #'here
                   `(let ,(for/list ([(expr var) (in-hash syntax-h)])
                            (list var expr))
                      ,main-expr))))
