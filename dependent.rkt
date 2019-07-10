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

(module structs racket/base
  (provide (all-defined-out))

  ;; An Alphabet is (alphabet Hash[TerminalSymbol => TokenKind])
  (struct alphabet (t=>kind fun=>arity) #:prefab)

  ;; A Grammar is (grammar NT (Listof Def))
  (struct grammar (start defs) #:prefab)

  ;; A Def is (def NT (Listof ElemSequence))
  (struct def (nt rhss) #:prefab)

  (struct ntelem (nt) #:prefab)
  (struct telem (t tr) #:prefab)
  )
(require 'structs)

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

(require (for-syntax racket/base
                     racket/syntax
                     (rename-in syntax/parse [attribute $])
                     'structs))
(begin-for-syntax
  (define (map-apply fs . xs) (for/list ([f (in-list fs)]) (apply f xs)))
  (define-syntax-class ntdef #:attributes (nt nt.ast mkast)
    #:description "nonterminal definition"
    (pattern [nt:symbol rhs:elemseq ...]
             #:attr mkast (lambda (nt?)
                            (def ($ nt.ast) (map-apply ($ rhs.mkast) nt?)))))
  (define-syntax-class elemseq #:attributes (mkast)
    #:description "element sequence"
    (pattern (e:elem ...)
             #:attr mkast (lambda (nt?)
                            (for/fold ([acc null] [venv null] #:result (reverse acc))
                                      ([e-mkast (in-list ($ e.mkast))] [var (in-list ($ e.name))])
                              (values (cons (e-mkast nt? venv) acc)
                                      (cons var venv))))))
  (define-syntax-class elem #:attributes (name mkast)
    #:description #f
    (pattern :t/nt #:attr name #f)
    (pattern [name:id :elem-content]))
  (define-syntax-class t/nt #:attributes (mkast)
    #:description #f
    (pattern s:symbol
             #:attr name #f
             #:attr mkast (lambda (nt? venv)
                            (cond [(nt? ($ s.ast)) (ntelem ($ s.ast))]
                                  [else (telem ($ s.ast) 'default)]))))
  (define-splicing-syntax-class elem-content #:attributes (mkast)
    #:description "element content"
    (pattern (~seq :t/nt))
    (pattern (~seq t:symbol #:kind tk:symbol)
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tk to list to check at runtime?
                            (cond [(nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol")]
                                  [else (telem ($ t.ast) ($ tk.ast))])))
    (pattern (~seq t:symbol #:call (tf:symbol arg:texpr ...))
                            ;; FIXME: add tf and arity to list to check at runtime?
             #:attr mkast (lambda (nt? venv)
                            (cond [(nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol")]
                                  [else (telem ($ t.ast)
                                               (cons ($ tf.ast) (map-apply ($ arg.mkast) venv)))]))))
  (define-syntax-class texpr #:attributes (mkast)
    #:description "token-function argument"
    #:literals (quote)
    (pattern var:id
             #:attr mkast (lambda (venv)
                            (or (for/or ([id (in-list venv)] [index (in-naturals 1)])
                                  (and id (bound-identifier=? #'var id) index))
                                (wrong-syntax #'var "unbound element variable"))))
    (pattern (quote datum)
             #:attr mkast (lambda (venv) `(quote ,(syntax->datum #'datum)))))
  (define-syntax-class symbol #:attributes (ast)
    (pattern x:id #:attr ast (syntax-e #'x)))
  (void))

(define-syntax DGrammar
  (syntax-parser
    [(_ #:start start:symbol d:ntdef ...)
     (define (nt? s) (member s ($ d.nt.ast)))
     (unless (nt? ($ start.ast)) (wrong-syntax #'start "expected nonterminal symbol"))
     #`(quote #,(grammar ($ start.ast) (map-apply ($ d.mkast) nt?)))]))

;; ============================================================

;; A Token is (cons Symbol Any); the symbol is the Terminal name.
(define (tok-t tok) (car tok))

(define EOF (string->uninterned-symbol "EOF"))
(define EOF-tok (list EOF))

;; ============================================================
;; ============================================================

(define dgrammar-base%
  (class object%
    (init g)
    (field [start (grammar-start g)]
           [defs (grammar-defs g)])
    (super-new)

    ;; ----------------------------------------

    (define/public (generate-corpus n)
      (for/list ([i (in-range n)]) (generate start)))

    (define/public (generate [want start])
      (cond [(nt? want)
             (define rhss (nt-rhss want))
             (append*
              (for/list ([sym (list-ref rhss (random (length rhss)))])
                (generate sym)))]
            [else `((,want))]))

    ;; ----------------------------------------

    (field [nt-h (for/hash ([def defs]) (values (def-nt def) (def-rhss def)))])

    (define/public (nt? sym) (and (hash-ref nt-h sym #f) #t))
    (define/public (terminal? sym) (and (symbol? sym) (not (nt? sym))))

    (define/public (nt-rhss nt)
      (or (hash-ref nt-h nt #f) (error 'nt-rhss "undefined nonterminal: ~e" nt)))

    ;; ----------------------------------------

    (define nt-nullable-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def defs])
           (hash-set h (def-nt def)
                     (for/or ([item (def-rhss def)]) (item-nullable? item #:h h)))))
       (hash)))

    ;; *-nullable? : ... -> Boolean
    (define/public (nt-nullable? sym #:h [h nt-nullable-h])
      (hash-ref h sym #t))
    (define/public (elem-nullable? elem #:h [h nt-nullable-h])
      (match elem [(ntelem nt) (nt-nullable? nt #:h h)] [_ #f]))
    (define/public (item-nullable? item #:h [h nt-nullable-h])
      (for/and ([elem item]) (elem-nullable? elem #:h h)))

    ;; ----------------------------------------

    (define MAX-MINLEN 20)
    (define nt-minlen-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def defs])
           (hash-set h (def-nt def)
                     (apply min MAX-MINLEN
                            (for/list ([item (def-rhss def)])
                              (item-minlen item #:h h))))))
       (hash)))

    ;; *-minlen : ... -> Nat
    (define/public (nt-minlen nt #:h [h nt-minlen-h])
      (hash-ref h nt 0))
    (define/public (elem-minlen elem #:h [h nt-minlen-h])
      (match elem [(ntelem nt) (nt-minlen nt #:h h)] [_ 1]))
    (define/public (item-minlen item #:h [h nt-minlen-h])
      (for/sum ([elem item]) (elem-minlen elem #:h h)))

    ;; ----------------------------------------

    (define nt-first-h
      (fixpoint
       (lambda (h)
         (for/hash ([def defs])
           (values (def-nt def)
                   (apply set-union '()
                          (for/list ([item (def-rhss def)])
                            (item-first item #:h h))))))
       (hash)))

    ;; *-first : ... -> (Listof telem)
    (define/public (nt-first nt #:h [h nt-first-h])
      (hash-ref h nt null))
    (define/public (elem-first elem #:h [h nt-first-h])
      (match elem [(ntelem nt) (nt-first nt #:h h)] [t (list t)]))
    (define/public (item-first item #:h [h nt-first-h])
      (let loop ([item item])
        (match item
          [(cons elem1 item2)
           (set-union (elem-first elem1 #:h h)
                      (if (elem-nullable? elem1) (loop item2) null))]
          ['() null])))

    ;; ----------------------------------------

    (define nt-final-h
      (fixpoint
       (lambda (h)
         (for/hash ([def defs])
           (values (def-nt def)
                   (apply set-union '()
                          (for/list ([item (def-rhss def)])
                            (item-final item #:h h))))))
       (hash)))

    ;; *-final : ... -> (Listof telem)
    (define/public (nt-final nt #:h [h nt-final-h])
      (hash-ref h nt null))
    (define/public (elem-final elem #:h [h nt-final-h])
      (match elem [(ntelem nt) (nt-final nt #:h h)] [t (list t)]))
    (define/public (item-final item #:h [h nt-final-h])
      (let loop ([item item])
        (match item
          [(cons elem1 item2)
           (set-union (elem-final elem1 #:h h)
                      (if (elem-nullable? elem1) (loop item2) null))]
          ['() null])))

    ;; ----------------------------------------

    (define nt-follow-h
      (fixpoint
       (lambda (h)
         (for*/fold ([h h]) ([def defs] [item (def-rhss def)])
           (for/fold ([h h] [follows-this (hash-ref h (def-nt def) null)] #:result h)
                     ([elem (reverse item)])
             (match elem
               [(ntelem nt)
                (values (hash-set h nt (set-union (hash-ref h nt null) follows-this))
                        (set-union (nt-first nt)
                                   (if (nt-nullable? nt) follows-this null)))]
               [_ (values h (list elem))]))))
       (hash start (list EOF))))

    ;; *-follow : ... -> (Listof telem)
    (define/public (nt-follow nt #:h [h nt-follow-h])
      (hash-ref h nt null))

    ;; ----------------------------------------

    (define/public (print)
      (printf "Nullable:\n")
      (pretty-print nt-nullable-h)
      (printf "Min length:\n")
      (pretty-print nt-minlen-h)
      (printf "First:\n")
      (pretty-print nt-first-h)
      (printf "Follow:\n")
      (pretty-print nt-follow-h))
    ))

;; ============================================================
;; ============================================================

(define START (string->uninterned-symbol "START"))

(define (lr-adjust-grammar g)
  (match-define (grammar start defs) g)
  (grammar START (cons (def START (list (list start EOF))) defs)))

(define DOT (string->uninterned-symbol "◇"))
(define (DOT? x) (eq? x DOT))

;; An LR0-Item is an Item that contains exactly one DOT symbol.
;; An LR0-Prod is (prod NT Nat LR0-Item)
(struct prod (nt index item) #:prefab)

(define (lritem-elem-after-dot lritem)
  (let loop ([lritem lritem])
    (cond [(DOT? (car lritem)) (and (pair? (cdr lritem)) (cadr lritem))]
          [else (loop (cdr lritem))])))

(define (lritem-dot-initial? lritem) (DOT? (car lritem)))
(define (lritem-dot-final? lritem) (DOT? (last lritem)))

(define (lrprod-elem-after-dot lrp)
  (lritem-elem-after-dot (prod-item lrp)))

(define (lrprod-dot-initial? lrp) (lritem-dot-initial? (prod-item lrp)))
(define (lrprod-dot-not-initial? lrp) (not (lrprod-dot-initial? lrp)))
(define (lrprod-dot-final? lrp) (lritem-dot-final? (prod-item lrp)))

(define (lrprod-advance-dot lrp)
  (match-define (prod nt index lritem) lrp)
  (prod nt index
        (let loop ([lritem lritem])
          (cond [(DOT? (car lritem)) (list* (cadr lritem) DOT (cddr lritem))]
                [else (cons (car lritem) (loop (cdr lritem)))]))))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (define (prod->label lrp)
    (match lrp [(prod nt index lritem) (list* nt index '→ lritem)]))
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (super-new [g (lr-adjust-grammar g)])
    (inherit-field start nt-h)
    (inherit nt? terminal? nt-follow)

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt rhss) (in-hash nt-h)])
        (values nt (for/list ([item rhss] [index (in-naturals)])
                     (prod nt index (cons DOT item))))))

    (define/public (nt-lrprods nt)
      ;; Returns the list of { NT -> DOT <Item> } productions (that is, DOT-first)
      (hash-ref nt-lrprods-h nt))

    (define/public (lrprod-children lrp)
      (define dotted-elem (lrprod-elem-after-dot lrp))
      (match (lrprod-elem-after-dot lrp)
        [(ntelem nt) (nt-lrprods nt)]
        [_ null]))

    (define state-h (make-hash)) ;; intern table
    (define-syntax-rule (in-states) (in-hash-keys state-h))

    (define/public (state-closure lrps)
      (define state (list-closure lrps (lambda (lrp) (lrprod-children lrp))))
      (hash-ref! state-h state (lambda () state)))

    (define (states-goto-closure state0)
      (define (state-goto-children state)
        (define groups ;; Hash[Symbol => (Listof LR0-Prod)]
          (for/fold ([h (hash)]) ([lrp (in-list state)])
            (cond [(lrprod-elem-after-dot lrp)
                   => (lambda (elem)
                        (hash-set h elem (cons lrp (hash-ref h elem null))))]
                  [else h])))
        (for/hash ([(elem lrps) (in-hash groups)])
          (values elem (state-closure (map lrprod-advance-dot (reverse lrps))))))
      (closure (list state0) state-goto-children #:worklist hash-values))

    (define state0 (state-closure (nt-lrprods start)))
    (define state-goto-h (states-goto-closure state0))

    (define (shift-state? state)
      (let ([goto-h (hash-ref state-goto-h state)])
        (for/or ([sym (in-hash-keys goto-h)]) (telem? sym))))
    (define (reduce-state? state)
      (for/or ([lrp (in-list state)])
        (lrprod-dot-final? lrp)))
    (define (state-goto state sym)
      (hash-ref (hash-ref state-goto-h state) sym #f))

    (define lr0-conflicts
      (append*
       (for/list ([state (in-states)])
         (append
          (if (and (shift-state? state) (reduce-state? state))
              `((shift-reduce ,state)) null)
          (if (< 1 (for/sum ([lrp (in-list state)])
                     (if (lrprod-dot-final? lrp) 1 0)))
              `((reduce-reduce ,state)) null)))))

    ;; ========================================

    (define/private (make-pstates/conflicts)
      (define conflicts null) ;; mutated, (Listof (list T (U 'shift Red) Red))
      (define next-index 0)
      (define (get-index)
        (begin0 next-index (set! next-index (add1 next-index))))
      (define (next-states st) (hash-values (hash-ref state-goto-h st)))
      (define state=>index
        (closure (list state0) next-states #:store (lambda (x) (get-index))))
      (define pstates (make-vector next-index))
      (define (state->pstate st index)
        (define label (lr-state-label st))
        (define shift (for/hash ([(sym st) (in-hash (hash-ref state-goto-h st))]
                                 #:when (terminal? sym))
                        (values sym (hash-ref state=>index st))))
        (define goto (for/hash ([(sym st) (in-hash (hash-ref state-goto-h st))]
                                #:when (nt? sym))
                       (values sym (hash-ref state=>index st))))
        ;; FIXME: intern shift, goto?
        (define reduce
          (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
            (match-define (prod nt index lritem) lrp)
            (list nt index (sub1 (length lritem)))))
        (define accept
          (cond [(equal? (map car reduce) (list START)) 'true]
                [(equal? (hash-keys shift) (list EOF)) 'virtual]
                [else #f]))
        (define reduce-lookahead (make-reduce-lookahead st index shift reduce))
        (pstate index label shift reduce goto accept reduce-lookahead))
      (define (make-reduce-lookahead st index shift reduce)
        (cond [(null? reduce) #f]
              [(and (hash-empty? shift) (<= (length reduce) 1)) #f]
              [else
               (define reduce-lookahead
                 (for/fold ([h (hash)]) ([red (in-list reduce)])
                   (match-define (list red-nt _ _) red)
                   (define follows (nt-follow red-nt))
                   (for/fold ([h h]) ([t (in-list follows)])
                     (cond [(hash-ref shift t #f)
                            (begin (push! conflicts (list t 'shift red)) h)]
                           [(hash-ref h t #f)
                            (begin (push! conflicts (list t (hash-ref h t) red)) h)]
                           [else (hash-set h t red)]))))
               (when (pair? conflicts)
                 (void))
               reduce-lookahead]))
      (for ([(st index) (in-hash state=>index)])
        (vector-set! pstates index (state->pstate st index)))
      (values pstates conflicts))

    (define-values (pstates pconflicts) (make-pstates/conflicts))
    (define/public (get-pstates) pstates)
    (define/public (get-pconflicts) pconflicts)

    (define/public (lr0-parse toks)
      ;; FIXME: check for conflicts!
      (lr0-parse* pstates toks))

    ;; ========================================

    (define/override (print)
      (super print)
      (when #t
        (printf "LR0 States:\n")
        ;; (pretty-print state-goto-h)
        (pretty-print pstates))
      (when (pair? lr0-conflicts)
        (printf "LR0 Conflicts:\n")
        (pretty-print lr0-conflicts))
      (when (pair? pconflicts)
        (printf "?? Conflicts:\n")
        (pretty-print pconflicts)))
    ))

(define-syntax-rule (push! var elem) (set! var (cons elem var)))

;; ============================================================

;; PState = (pstate Nat Any PShiftTable PReduce PGotoTable PAccept PReduceLookahead)
;; PShiftTable = Hash[TerminalSymbol => Nat]
;; PReduce = (Listof (list NT Nat Nat))
;;   PReduceLookahead = #f | Hash[TerminalSymbol => (list NT Nat Nat)]
;; PGotoTable = Hash[NT => Nat]
;; PAccept = (U #f 'true 'virtual)

;; A "true" accept state is a reduce state
;; - whose LR0-Prod's NT is START, or equivalently (by construction),
;; - whose LR0-Item ends with [EOF DOT].
;; A "virtual" accept state is a shift state
;; - whose only shift edge is EOF.

;; Deterministic LR(0)

(struct pstate (index label shift reduce goto accept reduce-lookahead) #:prefab)

(define (lr0-parse* states toks)
  (define (loop toks stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "\nSTATE = #~v\n" (car stack))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (cadr (cddr stack))]
                  [(virtual) (cadr stack)]))]
          [(pstate-reduce-lookahead st)
           => (lambda (reduce-lookahead)
                (define next-tok (if (pair? toks) (car toks) EOF-tok))
                (cond [(hash-ref reduce-lookahead (tok-t next-tok) #f)
                       => (lambda (red) (reduce st toks stack red))]
                      [else (shift st toks stack)]))]
          [(pair? (pstate-reduce st)) ;; (FIXME: assumes no conflicts!)
           (reduce st toks stack (car (pstate-reduce st)))]
          ;; otherwise, shift state (FIXME: assumes no conflicts!)
          [else (shift st toks stack)]))

  (define (reduce st toks stack red)
    (match-define (list nt index arity) red)
    (define-values (args stack*) (pop-values arity stack))
    (define value (list* nt index args))
    ;; (eprintf "REDUCE: ~v\n" value)
    (goto toks value stack*))

  (define (shift st toks stack)
    (define next-tok (if (pair? toks) (car toks) EOF-tok))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                ;; (eprintf "SHIFT ~v, #~s\n" next-tok next-state)
                (loop (if (pair? toks) (cdr toks) toks)
                      (list* next-state next-tok stack)))]
          [else (error 'lr0-parse "next = ~v, state = ~v" next-tok (car stack))]))

  (define (goto toks reduced stack)
    (define st (vector-ref states (car stack)))
    ;; (eprintf "RETURN VIA #~s\n" (car stack))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    ;; (eprintf "GOTO ~v\n" next-state)
    (loop toks (list* next-state reduced stack)))
  (loop toks (list 0)))

(define (pop-values arity stack)
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))
