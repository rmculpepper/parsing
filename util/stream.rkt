#lang racket/base
(require racket/match)
(provide (all-defined-out))

;; (Streamof X) = null | (cons X (-> (Streamof X)))

(define-syntax-rule (s-cons x y)
  (cons x (lambda () y)))

(define (s-list x) (s-cons x null))

(define (s-append xs ys)
  (match xs
    ['() (ys)]
    [(cons x xs) (s-cons x (s-append (xs) ys))]))

(define (s-bind xs f)
  (match xs
    ['() null]
    [(cons x xs) (s-append (f x) (lambda () (s-bind (xs) f)))]))

(define (s-iota a b) ;; returns [a ... b]
  (if (<= a b) (s-cons a (s-iota (add1 a) b)) null))

(define (s->list xs)
  (match xs
    ['() null]
    [(cons x xs) (cons x (s->list (xs)))]))
