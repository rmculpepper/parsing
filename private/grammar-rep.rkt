#lang racket/base
(provide (all-defined-out))

;; A Grammar is (grammar NT (Listof Def))
(struct grammar (start defs) #:prefab)

;; A Def is (def NT (Listof Prod))
(struct def (nt rhss) #:prefab)

;; A Prod is (prod NT Nat ElemSequence Action)
(struct prod (nt index item action) #:prefab)

;; An ElemSequence is (Listof Element)
;; An Element is one of
;; - (ntelem NT)
;; - (telem Terminal TokenReader)
(struct ntelem (nt) #:prefab)
(struct telem (t tr) #:prefab)

;; A Nonterminal (NT) is a Symbol

;; A Terminal is one of the following:
;; - symbol
;; - character
;; - boolean
;; - exact-integer
(define (ok-terminal? v)
  (or (symbol? v) (char? v) (boolean? v) (exact-integer? v)))

;; The TokenReader type is defined by the parser framework.
