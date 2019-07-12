#lang racket/base
(provide (all-defined-out))

;; A Grammar is (grammar NT (Listof Def) (Vectorof Any))
(struct grammar (start defs vals) #:prefab)

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

;; A TokenReader (during table construction) is one of
;; - (cons Symbol (Listof TExpr))
;; - (cons '#:apply Symbol Nat (Listof TExpr)) -- Nat is index into value table
;; where TExpr = Nat | (list Datum) -- a stack index or a literal value.
