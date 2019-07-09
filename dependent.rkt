#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "util/misc.rkt")
(provide (all-defined-out))

;; Kinded Tokens and Dependent Grammars

;; A TokenKind is a Symbol
;; The default TokenKind is 'default
;; The pseudo-terminal EOF may belong to multiple token kinds.

;; An Alphabet is (alphabet Hash[TerminalSymbol => TokenKind])
(struct alphabet (t=>kind) #:prefab)
(struct dgrammar (start alpha defs) #:prefab)

;; A Def is (def NT (Listof Item) (Values -> Result))
;; An Item is (Listof GSymbol)
(struct def (nt rhss action) #:prefab)

;; FIXME: need a way of sneaking a value into Token position, to allow eg
;;   Msg ::= f:Flag (? (has-X-bit? f) X) Y
;; But we need to avoid question of expression equality. Maybe allow
;;   (? LimitedExpression) where LimitedExpression = DepVar | (Symbol DepVar)
;; and assume that the symbol is a deterministic function? Or do
;; identitifier comparison and replace with canonical (eq?)
;; identifier? Or add TokenFunctions, eg
;;   token function (has-X-bit? _) : Boolean
;;   Msg :: f:Flag [true (has-X-bit? f)] X Y
;;   Msg :: f:Flag [false (has-X-bit? f)] Y

;; In grammar specification, an Element can be
;;   NT                         -- short for [_ NT], if don't care about value
;;   [name NT]
;;   T                          -- short for [_ T], if don't care about value
;;   [name T]                   -- short for [name T : TK], where T has unique TK
;;   [name T : TK]
;;   [T : (TF Expr ...)]        -- short for [_ T : (TF Expr ...)], if we don't care about value
;;   [name T : (TF Expr ...)]   -- where Expr = name | (quote Datum)

;; Maybe a Token doesn't have a unique TokenKind, there are just
;; TokenReaders, and a Token has an optional default TokenReader.
;; Then, for example, {true, false} could be shared by many TokenFunctions.
;; Orthogonal: token may have payload, may not.

;; ============================================================

;; A Token is (cons Symbol Any); the symbol is the Terminal name.
(define (tok-t tok) (car tok))

(define EOF (string->uninterned-symbol "EOF"))
(define EOF-tok (list EOF))
