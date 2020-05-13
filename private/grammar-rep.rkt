#lang racket/base
(require racket/match
         racket/list)
(provide (all-defined-out)
         (all-from-out (submod "." common)))

(module common racket/base
  (provide (all-defined-out))

  ;; Note that the representations of Nonterminals and Terminals
  ;; overlap. Grammars must disambiguate between the two and prevent
  ;; actual overlaps.

  ;; A Nonterminal (NT) is a Symbol
  ;; Users SHOULD only use interned symbols.
  (define (ok-nonterminal? v) (symbol? v))

  ;; A Terminal is one of the following:
  ;; - symbol           -- interned or unreadable
  ;; - character
  ;; - boolean
  ;; - exact-integer
  ;; In particular, a Terminal is quotable and comparable with eqv?.
  (define (ok-terminal? v)
    (or (symbol? v) (char? v) (exact-integer? v)))
  (define EOI (string->unreadable-symbol "$EOI$"))

  ;; A user expression may refer to parameters and previous results. It is
  ;; translated to a function that takes then as arguments, along with indicators
  ;; of the arguments to supply.

  ;; UserExpr[X] is (expr:user UserFun[X] (Listof LocalExpr))
  ;; UserFun[X] = Nat -- index in value table to Syntax[Expr[(Listof ??) -> X]]
  ;; LocalExpr = Nat            -- reference to nth Value on stack
  ;;           | (list Datum)   -- constant

  ;; At run time, a grammar has an associated values vector, mapping UserFun
  ;; indexes etc to values.
  (struct expr:user (fun args) #:prefab))

(require 'common)

;; ----------------------------------------

;; A Grammar+ is (grammar+ Grammar NT EndSpec)
;; An EndSpec is either (NEListof Terminal) or #f.
(struct grammar+ (g start end) #:prefab)

;; A Grammar is (grammar (Listof Def) ValuesDesc)
(struct grammar (defs vals) #:prefab)

(define (grammar->nt? g)
  (define nts (map def-nt (grammar-defs g)))
  (lambda (s) (and (member s nts) #t)))

;; A Def is (def NT Nat (Listof Prod))
;; ctxn is the number of extra value slots of context that action routines get.
(struct def (nt ctxn rhss) #:prefab)

;; A Prod is (prod NT Nat ElemSequence Action)
(struct prod (nt index item action) #:prefab)

;; An ElemSequence is (Vectorof Element)
;; An Element is one of
;; - (ntelem NT)
;; - (telem Terminal TokenReaderSpec)
;; - (top-elem Terminal)    -- look at the top *value*
(struct ntelem (nt) #:prefab)
(struct telem (t tr) #:prefab)
(struct top-elem (t) #:prefab)

(define (t/top-elem? x) (or (telem? x) (top-elem? x)))
(define (t/top-elem-t x) (match x [(telem t _) t] [(top-elem t) t]))

;; ValuesDesc = (Vectorof Syntax[Expr])   -- at compile time
;; ValuesDesc = (Vectorof Value)          -- at run time

;; A TokenReader represents an instruction to the scanner about how to
;; read the next token.

;; A TokenReaderSpec is one of
;; - #f                         -- polymorphic (compatible with any reader)
;; - Symbol                     -- reader with no arguments
;; - (cons Symbol UserExpr)     -- reader with arguments
(define default-tr 'default)

;; simple-tr? : TokenReaderSpec -> Boolean
;; A simple TR has no arguments.
(define (simple-tr? v) (or (not v) (symbol? v)))

;; In general, in order to do a read, all items in a state (and across
;; all threads, for a GLR parser) must agree on what TokenReader to
;; use. It is useful to check agreement statically, especially for
;; deterministic parsers, but expression equivalence is undecidable in
;; general, so we can only approximate. This issue can be difficult
;; for classical production/clause-based notations. A more transparent
;; UserExpr representation might allow the static checker to equate
;; more expressions.

;; An alternative to static checking is dynamic checking or a hybrid.

(define (elems-consistent-tr who elems)
  (define proper-elems (filter telem-tr elems)) ;; ignore polymorphic tokens like EOI
  (match (group-by telem-tr proper-elems)
    [(list) default-tr]
    [(list group)
     (telem-tr (car group))]
    [groups
     (define kinds (map telem-tr (map car groups)))
     (error who "inconsistent token readers\n  readers: ~v" kinds)]))
