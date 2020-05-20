#lang info

;; pkg info

(define version "0.1")
(define collection "grrparse")
(define deps '("base" "grrparse-lib" "rackunit-lib"))
(define build-deps '("racket-doc" "scribble-lib" "grrparse-lib"))
(define implies '("grrparse-lib"))
(define pkg-authors '(ryanc))

;; collection info

(define name "grrparse")
(define scribblings '(("scribblings/grrparse.scrbl" (multi-page))))
;;(define compile-omit-paths '("examples"))
;;(define test-omit-paths '("examples"))
