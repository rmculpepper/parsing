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

  ;; A Grammar is (grammar NT (Listof Def))
  (struct grammar (start defs) #:prefab)

  ;; A Def is (def NT (Listof Prod))
  (struct def (nt rhss) #:prefab)

  ;; A Prod is (prod NT Nat ElemSequence Action)
  (struct prod (nt index item action) #:prefab)

  (struct ntelem (nt) #:prefab)
  (struct telem (t tr) #:prefab)
  )
(require 'structs)

;; In grammar specification, an Element can be
;;   NT                             -- short for [_ NT]
;;   [name NT]
;;   T                              -- short for [_ T]
;;   [name T]                       -- short for [name T #:read default]
;;   [name T #:read TK]
;;   [T #:read (TF Expr ...)]       -- short for [_ T #:read (TF Expr ...)]
;;   [name T #:read (TF Expr ...)]  -- where Expr = name | (quote Datum)
;;   [T #:apply (RacketVariable Expr ...)] -- short for [_ name T #:apply (RacketVariable Expr ...)]
;;   [name T #:apply (RacketVariable Expr ...)]

;; FIXME: add special case:
;;   [T #:= RacketExpr]
;; with following conditions:
;; - the preceding environments must be identical (up to bound-id=?), including indexes
;; and the following behavior:
;; - equal-looking expressions (E1, E2, ...) are considered equal and "interned" into
;;   a single syntax object: (if #t wE2 (begin wE2 ...)), where wEk is Ek wrapped with
;;   element variable bindings.
;; - the expression's value (if an acceptable atom) becomes the token with an empty payload
;;   (if the value is not an acceptable atom, it is replaced with a gensym'd 'bad-token-name

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
                     syntax/id-table
                     syntax/transformer
                     (rename-in syntax/parse [attribute $])
                     "util/datum-to-expr.rkt"
                     'structs))
(begin-for-syntax
  (define racket-intern-table (make-parameter #f)) ;; free-id-table[Id]
  (define (map-apply fs . xs) (for/list ([f (in-list fs)]) (apply f xs)))
  (define-syntax-class ntdef #:attributes (nt nt.ast mkast)
    #:description "nonterminal definition"
    (pattern [nt:symbol rhs:ntrhs ...]
             #:attr mkast (lambda (nt?)
                            (def ($ nt.ast)
                              (for/list ([rhs-mkast (in-list ($ rhs.mkast))] [index (in-naturals)])
                                (rhs-mkast ($ nt.ast) index nt?))))))
  (define-syntax-class ntrhs #:attributes (mkast)
    (pattern [es:elemseq a:action]
             #:attr mkast (lambda (nt index nt?)
                            (define-values (es-ast venv) (($ es.mkast) nt?))
                            (define a-ast (($ a.mkast) venv))
                            (prod nt index es-ast a-ast))))
  (define-splicing-syntax-class action #:attributes (mkast)
    #:description "action routine"
    (pattern (~seq #:> ~! e:expr ...+)
             #:attr mkast (lambda (venv) (wrap-expr #'(let () e ...) venv))))
  (define-syntax-class elemseq #:attributes (mkast)
    #:description "element sequence"
    (pattern (e:elem ...)
             #:attr mkast (lambda (nt?)
                            (for/fold ([acc null] [venv null] #:result (values (reverse acc) venv))
                                      ([e-mkast (in-list ($ e.mkast))] [var (in-list ($ e.name))])
                              (values (cons (e-mkast nt? venv) acc)
                                      (cons var venv))))))
  (define-syntax-class elem #:attributes (name mkast)
    #:description #f
    (pattern :t/nt #:attr name #f)
    (pattern [:name :elem-content])
    (pattern [:elem-content] #:attr name #f))
  (define-syntax-class name #:attributes (name)
    #:literals (_)
    (pattern (~and _ ~!) #:attr name #f)
    (pattern x:id #:attr name #'x))
  (define-syntax-class t/nt #:attributes (mkast)
    #:description #f
    (pattern s:symbol
             #:attr name #f
             #:attr mkast (lambda (nt? venv)
                            (cond [(nt? ($ s.ast)) (ntelem ($ s.ast))]
                                  [else (telem ($ s.ast) 'default)])))
    (pattern t:non-symbol-token-name
             #:attr name #f
             #:attr mkast (lambda (nt? venv) (telem ($ t.ast) 'default))))
  (define-splicing-syntax-class elem-content #:attributes (mkast)
    #:description "element content"
    (pattern (~seq t:token-name #:read tk:symbol)
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tk to list to check at runtime?
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (telem ($ t.ast) ($ tk.ast))))
    (pattern (~seq t:token-name #:read (tf:symbol arg:texpr ...))
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tf and arity to list to check at runtime?
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (telem ($ t.ast) (cons ($ tf.ast) (map-apply ($ arg.mkast) venv)))))
    (pattern (~seq t:token-name #:apply (re:id arg:texpr ...))
             #:attr mkast (lambda (nt? venv)
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (record-disappeared-uses #'re)
                            (let ([re (free-id-table-ref! (racket-intern-table) #'re #'re)])
                              (telem ($ t.ast) (list* '#:apply re (map-apply ($ arg.mkast) venv))))))
    (pattern (~seq :t/nt)))
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
  (define-syntax-class token-name #:attributes (ast)
    (pattern (~or :symbol :non-symbol-token-name)))
  (define-syntax-class non-symbol-token-name #:attributes (ast)
    (pattern (~and x (~fail #:unless (let ([v (syntax-e #'x)])
                                       (or (char? v) (boolean? v) (exact-integer? v)))))
             #:attr ast (syntax-e #'x)))

  (struct token-variable (vvar tvar)
    #:property prop:procedure
    (lambda (self stx)
      (let ([vvar (token-variable-vvar self)] [tvar (token-variable-tvar self)])
        ((make-variable-like-transformer #`(get-token-value '#,vvar #,tvar)) stx))))

  (define (wrap-expr expr venv)
    (define tok-vars (generate-temporaries venv))
    (define bindings
      (for/list ([tvar (in-list tok-vars)] [vvar (in-list (reverse venv))] #:when (identifier? vvar))
        #`[#,vvar (token-variable (quote-syntax #,vvar) (quote-syntax #,tvar))]))
    #`(lambda #,tok-vars (letrec-syntax #,bindings #,expr)))
  (void))

(define-syntax DGrammar
  (syntax-parser
    [(_ #:start start:symbol d:ntdef ...)
     (define (nt? s) (member s ($ d.nt.ast)))
     (unless (nt? ($ start.ast)) (wrong-syntax #'start "expected nonterminal symbol"))
     (parameterize ((racket-intern-table (make-free-id-table)))
       (datum->expression (grammar ($ start.ast) (map-apply ($ d.mkast) nt?))
                          (lambda (v) (cond [(syntax? v) v] [else #f]))))]))

;; ============================================================

;; ============================================================

;; A Token value is a list containing the token name and an optional payload.
(define (tok-t tok) (car tok))

(define (tok-value tok)
  (match tok
    [(list t) #f]
    [(list* t v _) v]))

(define (get-token-value who tok)
  (match tok
    [(list t) (error who "token has no payload\n  token: ~e" t)]
    [(list* t v _) v]))

(define EOF (string->uninterned-symbol "EOF"))
(define EOF-tok (list EOF))

;; ============================================================
;; ============================================================

(define grammar-base%
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

    (define/public (nt-rhss nt)
      (or (hash-ref nt-h nt #f) (error 'nt-rhss "undefined nonterminal: ~e" nt)))

    ;; ----------------------------------------

    (define nt-nullable-h
      (fixpoint
       (lambda (h)
         (for/fold ([h h]) ([def defs])
           (hash-set h (def-nt def)
                     (for/or ([p (def-rhss def)])
                       (item-nullable? (prod-item p) #:h h)))))
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
                            (for/list ([p (def-rhss def)])
                              (item-minlen (prod-item p) #:h h))))))
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
                          (for/list ([p (def-rhss def)])
                            (item-first (prod-item p) #:h h))))))
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
                          (for/list ([p (def-rhss def)])
                            (item-final (prod-item p) #:h h))))))
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
         (for*/fold ([h h]) ([def defs] [p (def-rhss def)])
           (for/fold ([h h] [follows-this (hash-ref h (def-nt def) null)] #:result h)
                     ([elem (reverse (prod-item p))])
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

;; FIXME: need to treat EOF specially!
(define EOF-elem (telem EOF #f))

(define (lr-adjust-grammar g)
  (match-define (grammar start defs) g)
  (define start-p (prod START 0 (list (ntelem start) EOF-elem) (lambda (s e) s)))
  (grammar START (cons (def START (list start-p)) defs)))

(define DOT (string->uninterned-symbol "◇"))
(define (DOT? x) (eq? x DOT))

;; An LR0-Item is an Item that contains exactly one DOT symbol.
;; An LR0-Prod is (prod NT Nat LR0-Item Action)

(define (lritem-elem-after-dot lritem)
  (let loop ([lritem lritem])
    (cond [(DOT? (car lritem)) (and (pair? (cdr lritem)) (cadr lritem))]
          [else (loop (cdr lritem))])))

(define (lritem-dot-initial? lritem) (DOT? (car lritem)))
(define (lritem-dot-final? lritem) (DOT? (last lritem)))

(define (prod->initial-lrprod p)
  (match-define (prod nt index item action) p)
  (prod nt index (cons DOT item) action))

(define (lrprod-elem-after-dot lrp)
  (lritem-elem-after-dot (prod-item lrp)))

(define (lrprod-dot-initial? lrp) (lritem-dot-initial? (prod-item lrp)))
(define (lrprod-dot-not-initial? lrp) (not (lrprod-dot-initial? lrp)))
(define (lrprod-dot-final? lrp) (lritem-dot-final? (prod-item lrp)))

(define (lrprod-advance-dot lrp)
  (match-define (prod nt index lritem action) lrp)
  (prod nt index
        (let loop ([lritem lritem])
          (cond [(DOT? (car lritem)) (list* (cadr lritem) DOT (cddr lritem))]
                [else (cons (car lritem) (loop (cdr lritem)))]))
        action))

;; A LR0-State is (Listof LR0-Prod)

(define (lr-state-label st)
  (let ([base-lrps (filter lrprod-dot-not-initial? st)])
    (cond [(null? base-lrps) (prod->label (car st))]
          [(null? (cdr base-lrps)) (prod->label (car base-lrps))]
          [else (map prod->label base-lrps)])))
(define (prod->label lrp)
  (match lrp [(prod nt index lritem _) (list* nt index '→ (map elem->label lritem))]))
(define (elem->label elem)
  (match elem
    [(? DOT?) elem]
    [(ntelem nt) nt]
    [(telem t 'default) t]
    [(telem t #f) t]
    [(telem t tr) (list t tr)]))


(define debug-consistent #f)

(define (telems-consistent-tr elems [fail #f])
  (define proper-elems (filter telem-tr elems)) ;; ignore polymorphic tokens like EOF
  (match (group-by telem-tr proper-elems)
    [(list) #f]
    [(list group)
     (telem-tr (car group))]
    [groups
     (define kinds (map telem-tr (map car groups)))
     (set! debug-consistent kinds)
     (if fail (fail kinds) (error 'dep-lr "inconsistent token kinds\n  kinds: ~v" kinds))]))

;; ------------------------------------------------------------

(define (LR-mixin base%)
  (class base%
    (init g)
    (super-new [g (lr-adjust-grammar g)])
    (inherit-field start nt-h)
    (inherit nt? nt-follow)

    ;; ----------------------------------------

    (define nt-lrprods-h
      (for/hash ([(nt prods) (in-hash nt-h)])
        (values nt (map prod->initial-lrprod prods))))

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
      (define conflicts null) ;; mutated, (Listof ...)
      (define next-index 0)
      (define (get-index)
        (begin0 next-index (set! next-index (add1 next-index))))
      (define (next-states st) (hash-values (hash-ref state-goto-h st)))
      (define state=>index
        (closure (list state0) next-states #:store (lambda (x) (get-index))))
      (define pstates (make-vector next-index))
      (define (state->pstate st index)
        (define label (lr-state-label st))
        (define shift (for/hash ([(elem st) (in-hash (hash-ref state-goto-h st))]
                                 #:when (telem? elem))
                        (values elem (hash-ref state=>index st))))
        (define goto (for/hash ([(elem st) (in-hash (hash-ref state-goto-h st))]
                                #:when (ntelem? elem))
                       (values elem (hash-ref state=>index st))))
        ;; FIXME: intern shift, goto?
        (define reduce
          (for/list ([lrp (in-list st)] #:when (lrprod-dot-final? lrp))
            (match-define (prod nt index lritem action) lrp)
            (list nt index (sub1 (length lritem)) action)))
        (define accept
          (cond [(equal? (map car reduce) (list START)) 'true]
                [(equal? (hash-keys shift) (list EOF-elem)) 'virtual]
                [else #f]))
        (define reduce-lookahead (make-reduce-lookahead st index shift reduce))
        (define treader
          (telems-consistent-tr
           (append (hash-keys shift) (if reduce-lookahead (hash-keys reduce-lookahead) null))))
        (define shift* (for/hash ([(elem st) (in-hash shift)]) (values (telem-t elem) st)))
        (define goto* (for/hash ([(elem st) (in-hash goto)]) (values (ntelem-nt elem) st)))
        (define reduce-lookahead*
          (and reduce-lookahead
               (for/hash ([(elem red) (in-hash reduce-lookahead)])
                 (values (telem-t elem) red))))
        (pstate index label treader shift* reduce goto* accept reduce-lookahead*))
      (define (make-reduce-lookahead st index shift reduce)
        (cond [(null? reduce) #f]
              [(and (hash-empty? shift) (<= (length reduce) 1)) #f]
              [else
               (define reduce-lookahead
                 (for/fold ([h (hash)]) ([red (in-list reduce)])
                   (match-define (list red-nt _ _ _) red)
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
      ;; FIXME: if lookaheads, should be consistent with goto successors (??)
      (values pstates conflicts))

    (define-values (pstates pconflicts) (make-pstates/conflicts))
    (define/public (get-pstates) pstates)
    (define/public (get-pconflicts) pconflicts)

    (define/public (lr0-parse get-token)
      ;; FIXME: check for conflicts!
      (lr0-parse* pstates get-token))

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
        (printf "SLR Conflicts:\n")
        (pretty-print pconflicts)))
    ))

(define-syntax-rule (push! var elem) (set! var (cons elem var)))

;; ============================================================

;; PState = (pstate Nat Any TReader PShiftTable PReduce PGotoTable PAccept PReduceLookahead)
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

(struct pstate (index label tr shift reduce goto accept reduce-lookahead) #:prefab)

(define (apply->token f args)
  (define v (apply f args))
  (list (if (token-name? v) v 'bad-token-name)))

(define (token-name? v)
  (or (symbol? v) (exact-integer? v) (boolean? v) (char? v)))

(define (lr0-parse* states tz)
  (eprintf "STARTING PARSE\n")
  (define DEBUG? #f)
  (define (get-token peek? tr stack)
    (cond [(symbol? tr) (tz peek? tr null)]
          [(symbol? (car tr)) (tz peek? (car tr) (get-token-args (cdr tr) stack))]
          [(eq? (car tr) '#:apply) (apply->token (cadr tr) (get-token-args (cddr tr) stack))]
          [else (error 'lr0-parse "bad tr: ~e" tr)]))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        [`(quote ,datum) datum]
        [index
         (match (list-ref stack (+ index index -1))
           ;; FIXME: add tok-value abstraction
           [(list* t value _) value]
           [(list t) #f])])))

  (define (loop stack)
    (define st (vector-ref states (car stack)))
    (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (car stack) (pstate-label st)))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (cadr (cddr stack))]
                  [(virtual) (cadr stack)]))]
          [(pstate-reduce-lookahead st)
           => (lambda (reduce-lookahead)
                (define next-tok (get-token #t (pstate-tr st) stack))
                (cond [(hash-ref reduce-lookahead (tok-t next-tok) #f)
                       => (lambda (red) (reduce st stack red))]
                      [else (shift st stack)]))]
          [(pair? (pstate-reduce st)) ;; (FIXME: assumes no conflicts!)
           (reduce st stack (car (pstate-reduce st)))]
          ;; otherwise, shift state (FIXME: assumes no conflicts!)
          [else (shift st stack)]))

  (define (reduce st stack red)
    (match-define (list nt index arity action) red)
    (define-values (args stack*) (pop-values arity stack))
    (define value (list nt (apply action args))) ;; (list* nt index args)
    (when DEBUG? (eprintf "REDUCE: ~v\n" value))
    (goto value stack*))

  (define (shift st stack)
    (define next-tok (get-token #f (pstate-tr st) stack))
    (cond [(hash-ref (pstate-shift st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (when DEBUG? (eprintf "SHIFT ~v, #~s\n" next-tok next-state))
                (loop (list* next-state next-tok stack)))]
          ;; Accept pre-parsed non-terminals from the lexer too.
          [(hash-ref (pstate-goto st) (tok-t next-tok) #f)
           => (lambda (next-state)
                (loop (list* next-state next-tok stack)))]
          [else (error 'lr0-parse "next = ~v, state = ~v" next-tok (car stack))]))

  (define (goto reduced stack)
    (define st (vector-ref states (car stack)))
    (when DEBUG? (eprintf "RETURN VIA #~s\n" (car stack)))
    (define next-state (hash-ref (pstate-goto st) (car reduced)))
    (when DEBUG? (eprintf "GOTO ~v\n" next-state))
    (loop (list* next-state reduced stack)))
  (loop (list 0)))

(define (pop-values arity stack) ;; produces values in original order
  (let loop ([arity arity] [stack stack] [acc null])
    (if (zero? arity)
        (values acc stack)
        (loop (sub1 arity) (cddr stack) (cons (cadr stack) acc)))))

;; ============================================================

;; A Tokenizer is (Boolean Symbol (Listof Arg) -> Token).
;; The tokenizer should be aware of peek vs read, so that for example
;; on input ports it can implement token-peeking by port-peeking.

;; A SimpleTokenizer is (-> Token).

(define (peeking-tokenizer tz)
  (define peeked #f)
  (define (tokenize peek? kind args)
    (cond [peeked (begin0 peeked (unless peek? (set! peeked #f)))]
          [peek? (let ([v (tz #f kind args)]) (set! peeked v) v)]
          [else (tz #f kind args)]))
  tokenize)

(define (dispatch-tokenizer h)
  (define (tokenize peek? kind args)
    (cond [(hash-ref h kind #f)
           => (lambda (v)
                (cond [(procedure? v)
                       (if (null? args) (v) (dispatch-arity-error kind 0 args))]
                      [(= (car v) (length args)) (apply (cdr v) args)]
                      [else (dispatch-arity-error kind (car v) null)]))]
          [else (error 'tokenizer "unknown token kind\n  name: ~e" kind)]))
  tokenize)

(define (dispatch-arity-error kind arity args)
  (cond [(zero? arity)
         (error 'tokenizer
                (string-append "token kind used with arguments"
                               "\n  token kind: ~s"
                               "\n  given: ~e")
                kind args)]
        [else
         (error 'tokenizer
                (string-append "token function arity mismatch"
                               "\n  token function: ~s"
                               "\n  expected: ~s arguments"
                               "\n  given: ~e")
                kind arity args)]))

(define (get-char-token in #:token-name [tname 'char] #:special [special null])
  (define next (peek-char in))
  (cond [(eof-object? next) EOF-tok]
        [(memv next special) (begin (read-char in) (list next next))]
        [else (begin (read-char in) (list tname next))]))

(define (get-byte-token in #:token-name [tname 'byte] #:special [special null])
  (define next (peek-byte in))
  (cond [(eof-object? next) EOF-tok]
        [(memv next special) (begin (read-byte in) (list next next))]
        [else (begin (read-byte in) (list tname next))]))

(define (get-string-token in #:token-name [tname 'string] #:delimiters [delims null])
  (define next (peek-char in))
  (cond [(eof-object? next) EOF-tok]
        [else
         (define out (open-output-string))
         (let loop ()
           (define next (peek-char in))
           (cond [(or (eof-object? next) (memv next delims))
                  (list tname (get-output-string out))]
                 [else (begin (read-char in) (write-char next out) (loop))]))]))
