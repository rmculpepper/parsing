#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "grammar.rkt"
         "unger-parser.rkt"
         "ll1-parser.rkt"
         "lr-parser.rkt"
         "util/stream.rkt"
         "test.rkt")
(provide (all-defined-out))

;; ============================================================

(define unger-parser% (unger-mixin grammar-base%))

;; ============================================================

(eprintf "\nExample 1:\n")
(define up1 (new unger-parser% (g g1)))
(send up1 print)
(s->list (send up1 unger-parse s1a))

;; --------------------

(eprintf "\nExample 2:\n")
(define up2 (new unger-parser% (g g2)))
(send up2 print)
(s->list (send up2 unger-parse s2a))

;; --------------------

(eprintf "\nExample 3:\n")
(define up3 (new unger-parser% (g g3)))
(send up3 print)
(s->list (send gg3 unger-parse s3a))

;; ----------------------------------------

(when #t
  (time (for ([i (in-range #e1e4)])
          (s->list (send gg1 unger-parse s1a))))
  (time (for ([i (in-range #e1e5)])
          (s->list (send gg2 unger-parse s2a))))
  (void))

(when #t
  (printf "Parsing corpora using Unger parser\n")
  (time (for ([i (in-range #e1e1)])
          (for ([s (in-list corpus1)])
            (s->list (send up1 unger-parse s)))))
  (time (for ([i (in-range #e1e1)])
          (for ([s (in-list corpus2)])
            (eprintf "parsing (~s) ~v\n" (length s) s)
            (s->list (send up2 unger-parse s)))))
  (time (for ([i (in-range #e1e1)])
          (for ([s (in-list corpus3)])
            (s->list (send up3 unger-parse s)))))
  (void))
