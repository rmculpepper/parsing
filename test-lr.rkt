#lang racket/base
(require racket/class
         racket/pretty
         "grammar.rkt"
         "lr-parser.rkt"
         "util/stream.rkt"
         "test.rkt")
(provide (all-defined-out) (all-from-out "test.rkt"))

(pretty-print-columns 100)

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

;; --------------------

(eprintf "\nExample 5:\n")
(define p5 (new parser% (g g5)))
(send p5 print)

;; --------------------

(eprintf "\nExample 6:\n")
(define p6 (new parser% (g g6)))
(send p6 print)

;; ----------------------------------------
