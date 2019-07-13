#lang racket/base
(require racket/class
         "private/unger.rkt"
         "test.rkt")
(provide (all-defined-out)
         (all-from-out "test.rkt")
         (all-from-out "private/unger.rkt"))

;; ============================================================

;;(eprintf "\nExample 1:\n")
(define up1 (unger-parser #:grammar g1))

(define s1b '((noun) (verb)))
(define s1b2 '((adj) (noun) (verb)))

;; --------------------

;;(eprintf "\nExample 2:\n")
(define up2 (unger-parser #:grammar g2))

;; --------------------

;;(eprintf "\nExample 3:\n")
(define up3 (unger-parser #:grammar g3))
