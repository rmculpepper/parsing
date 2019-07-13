#lang racket/base
(require "grammar-rep.rkt")
(provide (all-defined-out))

;; ============================================================

;; UItem = ...
(struct unger-nts (nts) #:prefab)
(struct unger-split (left-uitem terminal right-uitem) #:prefab)

;; UGrammar = (Vectorof UNTInfo)
;; UNTInfo = (unger-nt Nat (Listof UProd) Boolean)
;; UProd = (prod NT Nat UItem Action)
(struct unger-nt (nt prods nullable?) #:prefab)
