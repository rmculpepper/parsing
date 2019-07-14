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

;; ----------------------------------------

(random-seed 17)
(printf "Generating corpora\n")
(define corpus1 (begin #;time (send up1 generate-corpus #e1e2)))
(define corpus2 (begin #;time (send up2 generate-corpus #e5e1)))
(define corpus3 (begin #;time (send up3 generate-corpus #e1e2)))

(time (for ([i (in-range 1)])
        (for ([s (in-list corpus2)])
          (printf "sentence: ~s tokens\n" (length s))
          (time (send up2 parse (map apply-tok* s))))))
