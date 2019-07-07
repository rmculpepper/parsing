#lang racket/base
(require racket/class
         "grammar.rkt"
         "lr-parser.rkt"
         "util/stream.rkt"
         "test.rkt")
(provide (all-defined-out) (all-from-out "test.rkt"))

;; ============================================================

(define parser% (LR-mixin grammar-base%))

;; ============================================================

(eprintf "\nExample 1:\n")
(define p1 (new parser% (g g1)))
(send p1 print)

;; --------------------

(eprintf "\nExample 2:\n")
(define p2 (new parser% (g g2)))
(send p2 print)

;; --------------------

(eprintf "\nExample 3:\n")
(define p3 (new parser% (g g3)))
(send p3 print)

;; --------------------

(eprintf "\nExample 4:\n")
(define p4 (new parser% (g g4)))
(send p4 print)

;; ----------------------------------------
