#lang racket/base
(require racket/match
         racket/class
         racket/list
         racket/pretty
         racket/set
         "util/misc.rkt")
(provide (all-defined-out))

;; Kinded Tokens and Dependent Grammars

;; A TokenKind is a Symbol.

;; A Token has a default TokenKind, which is used by the parser to
;; select the TokenReader for a set of expected Tokens. But a
;; TokenReader may produce other Tokens. (?? limit to declared ??)

;; The pseudo-terminal EOF may belong to multiple TokenKinds.

;; ?? Useful to limit membership of TokenKind explicitly?
;; ?? Useful to limit membership of TokenFunction results explicitly?

;; A Tokenizer has the following interface:
;; - read : (TokenKind -> Token) and (TokenFunction Any ... -> Token)
;; - peek : (TokenKind -> Token) -- functions not supported ??

;; ?? Don't allow functions in lookahead position?

;; An Alphabet is (alphabet Hash[TerminalSymbol => TokenKind])
(struct alphabet (t=>kind fun=>arity) #:prefab)
(struct dgrammar (start alpha defs) #:prefab)

;; A Def is (def NT (Listof Item) (... -> Result))
;; An Item is (Listof GSymbol)
(struct def (nt rhss action) #:prefab)

;; In grammar specification, an Element can be
;;   NT                         -- short for [_ NT], if don't care about value
;;   [name NT]
;;   T                          -- short for [_ T], if don't care about value
;;   [name T]                   -- short for [name T : TK], where T has unique TK
;;   [name T : TK]
;;   [T : (TF Expr ...)]        -- short for [_ T : (TF Expr ...)], if we don't care about value
;;   [name T : (TF Expr ...)]   -- where Expr = name | (quote Datum)

;; During table construction, an Element is one of
;; - (ntelem NT)
;; - (telem T TReader)
;;   where TReader is TKind or (list TFunction Expr ...)
;;   where TKind and TFunction are Symbols
;;   and Expr is Nat or (quote Datum) -- nat is stack index



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


;; FIXME: allow booleans, fixnums, and characters to be Tokens?
;; Booleans would make the example above nicer.

;; Orthogonal: token may have payload, may not.

;; Can a Tokenizer produce a nonterminal instance? Why not?

;; ============================================================

;; A Token is (cons Symbol Any); the symbol is the Terminal name.
(define (tok-t tok) (car tok))

(define EOF (string->uninterned-symbol "EOF"))
(define EOF-tok (list EOF))
