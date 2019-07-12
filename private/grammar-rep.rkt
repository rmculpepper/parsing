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

;; A TokenReader represents an instruction to the scanner about how to
;; read the next token.

;; A TerminalElement can be polymorphic: it is compatible with any
;; TokenReader. The EOF pseudo-terminal is polymorphic. A polymorphic
;; TerminalElement is represented by a TokenReader field of #f.

;; ?? Useful to limit membership of TokenKind explicitly?
;; ?? Useful to limit membership of TokenFunction results explicitly?
;; ?? Don't allow functions in lookahead position?
;; ?? Orthogonal: token may have payload, may not.
;; ?? Can a Tokenizer produce a nonterminal instance?

;; Some design rationale: We want a way of sneaking a value into Token
;; position, to allow eg
;;
;;   Msg ::= f:Flag (if (has-X-bit? f) then X else ε) Y
;;
;; But we need to avoid (or limit) the question of expression
;; equality, or else go from a "set of sequence" view of productions
;; to an "expression" view. So we don't allow arbitrary expressions;
;; instead, we require registration of functions and recognize them by
;; symbol name, and we limit arguments to element variables and quoted
;; constants. So for example:
;;
;;   token function (has-X-bit? _)
;;   Msg -> [f Flag] [true  : (has-X-bit? f)] X Y
;;   Msg -> [f Flag] [false : (has-X-bit? f)] Y

(define EOF (string->uninterned-symbol "EOF"))
