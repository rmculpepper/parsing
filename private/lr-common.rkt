#lang racket/base
(provide (all-defined-out))

;; PState = (pstate Nat Any TReader PShiftTable PReduce PGotoTable PAccept PLookahead)
;; PShiftTable = Hash[TerminalSymbol => Nat]
;; PReduce = (reduction NT Nat Nat Action)
;;   PLookahead = #f | Hash[TerminalSymbol => PReduce]
;; PGotoTable = Hash[NT => Nat]
;; PAccept = (U #f 'true 'virtual)

;; A "true" accept state is a reduce state
;; - whose LR0-Prod's NT is START, or equivalently (by construction),
;; - whose LR0-Item ends with [EOF DOT].
;; A "virtual" accept state is a shift state
;; - whose only shift edge is EOF.

(struct pstate (index label tr shift reduce goto accept lookahead) #:prefab)
(struct reduction (nt index arity action) #:prefab)

;; An LR0-Conflict is (conflict State #f Boolean (Listof Reduction))
;; A LookaheadConflict is (conflict State Terminal (U State #f) (Listof Reduction))
;; If the shift state is present, there are shift/reduce conflicts (and possibly also
;; reduce/reduce conflicts!); otherwise there are only reduce/reduce conflicts.
(struct conflict (state t shift reds) #:prefab)
