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
    (or (symbol? v) (char? v) (boolean? v) (exact-integer? v)))
  (define EOF (string->unreadable-symbol "EOF"))

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

;; A Grammar is (grammar NT (Listof Def) ValuesDesc)
(struct grammar (start defs vals) #:prefab)

;; A Def is (def NT (Listof Prod))
(struct def (nt rhss) #:prefab)

;; A Prod is (prod NT Nat ElemSequence Action)
(struct prod (nt index item action) #:prefab)

;; An ElemSequence is (Vectorof Element)
;; An Element is one of
;; - (ntelem NT)
;; - (telem Terminal TokenReaderSpec)
;; - (pure-elem Terminal UserExpr)
(struct ntelem (nt) #:prefab)
(struct telem (t tr) #:prefab)
(struct pure-elem (t expr) #:prefab)

;; A pure-elem represents a Racket computation that produces a Token without
;; reading from the input. The expression should be pure. It is beneficial to
;; know when two expressions are the same; it eliminates spurious conflicts. For
;; example, the following parser-expression
;;   (case/p <expr> [true pexpr1] [false pexpr2])
;; might be translated to the following productions:
;;   [( [true  : #:pure <expr>] pexpr1 ) _]
;;   [( [false : #:pure <expr>] pexpr2 ) _]
;; If we recognize that the two occurrences of <expr> are the same, we can
;; conclude that the two productions are mutually exclusive. But the equivalence
;; of Racket expressions is undecidable, so we must approximate.

;; ValuesDesc = (Vectorof Syntax[Expr])   -- at compile time
;; ValuesDesc = (Vectorof Value)          -- at run time

;; A TokenReader represents an instruction to the scanner about how to
;; read the next token.

;; A TokenReaderSpec is one of
;; - #f                         -- polymorphic (compatible with any reader)
;; - Symbol                     -- reader with no arguments
;; - (cons Symbol UserExpr)     -- reader with arguments
(define default-tr 'default)

;; In general, in order to do a read, all items in a state (and across
;; all threads, for a GLR parser) must agree on what TokenReader to
;; use. It is useful to check agreement statically, especially for
;; deterministic parsers, but expression equivalence is undecidable in
;; general, so we can only approximate. This issue can be avoided
;; (somewhat) by parser-expression front end, eg a case/p form, but it
;; is difficult for classical production/clause-based notations. A
;; more transparent UserExpr representation might allow the static
;; checker to equate more expressions.

;; An alternative to static checking is dynamic checking or a hybrid.

(define (telems-consistent-tr who elems [fail #f])
  (define proper-elems (filter telem-tr elems)) ;; ignore polymorphic tokens like EOF
  (match (group-by telem-tr proper-elems)
    [(list) default-tr]
    [(list group)
     (telem-tr (car group))]
    [groups
     (define kinds (map telem-tr (map car groups)))
     (if fail (fail kinds) (error who "inconsistent token readers\n  readers: ~v" kinds))]))


;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;;
;;   OLD
;;
;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
;;
;; A TokenReader (during table construction) is one of
;; - (cons Symbol (Listof TExpr))
;; - (cons '#:apply Symbol Nat (Listof TExpr)) -- Nat is index into value table
;; where TExpr = Nat | (list Datum) -- a stack index or a literal value.
;;
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
