#lang racket/base
(require racket/match
         "common.rkt")
(provide (all-defined-out))

;; PState = (pstate StIndex Label TR Shifts Reduces Gotos Lookahead)
;; StIndex = Nat
;; Label = Any
;; Shifts = Hash[TerminalSymbol => Nat]
;; Reduces = (Listof Reduction)
;; Lookahead = #f | Hash[TerminalSymbol => Reduces]
;; Gotos = Hash[NT => Nat]

(struct pstate (index label tr shift reduce goto lookahead) #:prefab)
(struct reduction (nt index arity ctxn action) #:prefab)

;; TR =
;; | TokenReaderSpec    -- read next token using given reader
;; | #f                 -- this state does not read; shift and lookahead = #f
;; | '#:top             -- use the value from the top of the stack; reduces = null

;; Reduction = (reduction NT Nat Nat Action)
;; Action = Nat     -- index in values vector
;;        | 'accept -- accept, return the top value

;; A Reduction carries its NT (and its production index, for debugging).
;; The arity indicates how many Token values should be popped from the
;; stack and given to the action routine (fetched from the values vector).


;; I'll describe the semantics of Shifts, Reduces, Lookahead, and
;; Gotos first in terms of a nondeterministic (eg, GLR) parser.
;;
;; Suppose that we have already parsed `prefix` and have just entered
;; state `st`, and the next token is `tok`, with terminal symbol `t`.
;;
;; - If shifts maps t to next-st, then (prefix || t) is a viable
;;   prefix. We could push tok, push next-st, and then enter next-st.
;;
;; - If reduces contains a reduction for NT, we could perform it
;;   (popping some number of slots off the stack) and then "return" to
;;   the new top of the stack with the reduced NT value.
;;
;; - If lookahead exists and maps t to a set of reductions, then that
;;   set contains all the viable reductions given (prefix || t). The
;;   lookahead table may overapproximate --- that is, after performing
;;   reductions and gotos we might enter a state where t is neither a
;;   valid shift nor a lookahead for a reduce.
;;
;; Suppose that we have returned to `st` with a reduced NT value.
;;
;; - The gotos table maps NT to next-st. We could push the NT-tagged
;;   value, push next-st, and then enter next-st.


;; A set of states can be used for deterministic parsing if there are
;; no conflicts of the following sorts:
;;
;; - Shift/Reduce(st, t): state st has a shift table containing t and
;;   either (1) a lookahead table with an entry for t or (2) no
;;   lookahead table and at least one reduction
;;
;; - Reduce/Reduce(st, t): some state has a lookahead table that maps
;;   t to a list of two or more reductions
;;
;; - Reduce/Reduce(st): some state has no lookahead table and a list
;;   of two or more reductions

;; A Conflict is one of
;; - (conflict:s/r Nat Terminal)
;; - (conflict:s/r Nat Terminal/#f)
(struct conflict:s/r (st t) #:prefab)
(struct conflict:r/r (st t) #:prefab)

;; ============================================================
;; Error reporting

(struct pretty-state (index label) #:prefab #:reflection-name 'state)

(define (convert-pretty-state v)
  (if (pstate? v) (pretty-state (pstate-index v) (pstate-label v)) v))

(struct lr-context (op vsk)
  #:methods gen:context
  [(define (context->stack self)
     (map convert-pretty-state (lr-context-vsk self)))
   (define (context->stacks self)
     (list (context->stack self)))
   (define (context->expected-terminals self)
     (match-define (lr-context op (list* v1 s2 _)) self)
     (if (eq? op 'top) #f (hash-keys (pstate-shift s2))))
   (define (context->error-lines self)
     (format "\n  expected one of: ~s\n  state: ~e"
             (context->expected-terminals self)
             (convert-pretty-state (cadr (lr-context-vsk self)))))])
