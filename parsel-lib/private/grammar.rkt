;; Copyright 2019-2024 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(provide (all-defined-out) (for-syntax (all-defined-out)))


;; ============================================================
;; Grammar representation

(module grammar-rep racket/base
  (provide (all-defined-out))
  ;; type parameters: Action

  ;; ----------------------------------------
  ;; NT Defs

  ;; A Def is (def NT (Listof Prod))
  (struct def (nt rhss) #:prefab)

  ;; A Prod is (prod NT Nat ElemSequence Action)
  (struct prod (nt index elems action) #:prefab)

  ;; An ElemSequence is (Vectorof Element)
  ;; An Element is either (ntelem NT) or (telem Terminal Boolean).
  (struct ntelem (nt) #:prefab)
  (struct telem (t value?) #:prefab)

  (define (value-elem? e)
    (or (ntelem? e) (and (telem? e) (telem-value? e))))

  ;; ----------------------------------------
  ;; Grammars

  ;; A Grammar is (grammar Symbols Symbols (Listof Def) Symbol/#f Symbols/#f)
  ;; FIXME: add precedence decls?
  (struct grammar (ets vts defs start endts) #:prefab)

  (void))

(module grammar-builder racket/base
  (require scramble/result)
  (provide (all-defined-out))

  ;; A GrammarBuilder is
  ;; (grammarbuilder (Hasheq Symbol => (U 'et 'vt Def)) Symbol/#f Symbols/#f)
  (struct grammarbuilder (h start endts) #:mutable)

  (define (make-grammarbuilder)
    (grammarbuilder (make-hasheq) #f #f))

  ;; gb-lookup : Symbol -> (U 'et 'vt Def)
  (define (gb-lookup gb sym)
    (hash-ref (grammarbuilder-h gb) sym #f))

  (define ((gb->lookup gb) sym) (gb-lookup gb sym))

  (define (gb-add gb names getv [stxs names] [xs names] #:conflict handle-conflict)
    (define h (grammarbuilder-h gb))
    (for ([name (in-list names)] [stx (in-list stxs)] [x (in-list xs)])
      (define v (hash-ref h name))
      (define impv (getv name x))
      (cond [(and v (eqv? v impv)) (void)]
            [else
             (when v (handle-conflict name stx x v impv))
             (hash-set! h name impv)])))

  (define (hash-set!/check h k v kstx handle-conflict)
    (define oldv (hash-ref h k #f))
    (when (and oldv (not (eqv? oldv v)))
      (handle-conflict k kstx oldv v))
    (hash-set! h k v))

  (define (gb-import-grammar gb impg handle-conflict)
    (define h (grammarbuilder-h gb))
    (match-define (grammar ets vts defs start endts) impg)
    (for ([et (in-list ets)])
      (hash-set!/check h et 'et #f handle-conflict))
    (for ([vt (in-list vts)])
      (hash-set!/check h vt 'vt #f handle-conflict))
    (for ([d (in-list defs)])
      (hash-set!/check h (def-nt d) d #f handle-conflict)))

  (define (gb-add-terminals gb value? syms stxs #:conflict handle-conflict)
    (define h (grammarbuilder-h gb))
    (for ([tsym (in-list syms)] [tstx (in-list stxs)])
      (hash-set!/check h tsym (if value? 'vt 'et) tstx handle-conflict)))

  (define (gb-add-nonterminals gb defs stxs #:conflict handle-conflict)
    (define h (grammarbuilder-h gb))
    (for ([d (in-list defs)] [dstx (in-list stxs)])
      (hash-set!/check h (def-nt d) d dstx handle-conflict)))

  (define (gb-set-start gb start)
    (set-grammarbuilder-start! gb start))

  (define (gb-set-endts gb endts)
    (set-grammarbuilder-endts! gb endts))

  (void))

;; ============================================================
;; Grammar support

(module grammar-support racket/base
  (require (for-syntax racket/base
                       racket/syntax
                       syntax/parse
                       syntax/datum)
           racket/stxparam
           "token.rkt")
  (provide (all-defined-out))

  (define-syntax-parameter $-info #f)

  (begin-for-syntax
    (define-syntax-class entry
      (pattern (nt? names))))

  (define-syntax $lambda
    (syntax-parser
      [(_ (ve:entry ...) expr)
       #:with (tmparg ...) (generate-temporaries #'(ve ...))
       #'(lambda (tmparg ...)
           (syntax-parameterize (($-info (list (cons (quote-syntax tmparg) (quote ve)) ...)))
             expr))]))

  (define-syntax ($ stx)
    (define info
      (or (syntax-parameter-value #'$-info)
          (raise-syntax-error #f "used out of context" stx)))
    (define (lookup ref)
      (for/or ([entry (in-list info)])
        (and (memv ref (caddr entry)) entry)))
    (define-syntax-class argref #:attributes (entry)
      (pattern name:id #:attr entry (lookup (syntax-e #'name)) #:when (datum entry))
      (pattern num:nat #:attr entry (lookup (syntax-e #'num)) #:when (datum entry)))
    (syntax-parse stx
      [(_ (~optional (~seq #:value)) a:argref)
       (unless (cadr (datum a.entry))
         (raise-syntax-error #f "terminal does not carry value" stx #'a))
       #`(token-value #,(car (datum a.entry)))]
      [(_ #:token a:argref)
       (car (datum a.entry))]
      [(_ #:name a:argref)
       #'(token-name ($ #:token a))]
      [(_ #:sources a:argref)
       #'(token-sources ($ #:token a))]
      [(_ #:start a:argref)
       #'(token-start ($ #:token a))]
      [(_ #:end a:argref)
       #'(token-end ($ #:token a))]
      [(_ #:all-tokens)
       (with-syntax ([(te ...) (map car info)])
         #'(list te ...))]
      [(_ #:all-start+end)
       #'(tokens-start+end ($ #:all-tokens))]
      ))

  (define ((auto-action nt index) toks)
    (list* nt index (map token-value (filter token-with-value? toks))))

  (void))

;; ============================================================
;; Grammar syntax: non-terminal definitions

(module grammar-ct-ntdefs racket/base
  (require racket/base
           racket/syntax
           syntax/transformer
           syntax/free-vars
           syntax/parse
           (only-in syntax/datum datum)
           (submod ".." grammar-rep))
  (provide (all-defined-out))

  (define lift-table (make-parameter null)) ;; (Listof Syntax[[Id Expr]])
  (define (lift! expr #:id [id (car (generate-temporaries '(grammar-lifted)))])
    (begin0 id (lift-table (cons (list id expr) (lift-table)))))

  ;; ----------------------------------------
  ;; Non-terminal definitions

  (define-syntax-class ntdef #:attributes (nt nt.ast mkast)
    #:description "nonterminal definition"
    (pattern [nt:symbol rhs:ntrhs ...]
             #:attr mkast (lambda (lookup)
                            (def (datum nt.ast)
                              (for/list ([rhs-mkast (in-list (datum (rhs.mkast ...)))]
                                         [index (in-naturals)])
                                (rhs-mkast (datum nt.ast) index lookup))))))

  (define-syntax-class ntrhs #:attributes (mkast)
    #:description "nonterminal production"
    (pattern [es:elemseq a:action]
             #:attr mkast (lambda (nt index lookup)
                            (define-values (elems venv) ((datum es.mkast) lookup))
                            (define action ((datum a.mkast) nt index venv))
                            (prod nt index elems action))))

  (define-splicing-syntax-class action #:attributes (mkast)
    #:description "action routine"
    (pattern (~seq #:auto ~!)
             #:attr mkast (lambda (nt index venv)
                            (lift! (make-auto-action nt index))))
    (pattern (~seq #:apply ~! proc:expr)
             #:attr mkast (lambda (nt index venv)
                            (lift! (make-proc-action #'proc))))
    (pattern (~seq (~optional (~seq #:> ~!)) e:expr ...+)
             #:attr mkast (lambda (nt index venv)
                            (lift! (make-expr-action venv #'(let () e ...))))))

  (define-syntax-class elemseq #:attributes (mkast)
    #:description "element sequence"
    (pattern (e:elem ...)
             #:attr mkast (lambda (lookup)
                            (define elems
                              (for/list ([e-mkast (datum (e.mkast ...))])
                                (e-mkast lookup)))
                            (values elems
                                    (for/list ([elem (in-list elems)]
                                               [index (in-naturals 1)]
                                               [name (in-list (datum (e.name ...)))])
                                      (list (value-elem? elem)
                                            (if name (list name index) (list index))))))))

  (define-syntax-class elem #:attributes (name mkast)
    #:description #f
    (pattern :t/nt #:attr name #f)
    (pattern [:name :t/nt]))

  (define-syntax-class name #:attributes (name)
    #:literals (_)
    (pattern (~and _ ~!) #:attr name #f)
    (pattern x:id #:attr name #'x))

  (define-syntax-class t/nt #:attributes (mkast)
    #:description #f
    (pattern s:symbol
             #:attr name #f
             #:attr mkast (lambda (lookup)
                            (match (lookup (datum s.ast))
                              ['et (telem (datum s.ast) #f)]
                              ['vt (telem (datum s.ast) #t)]
                              [(? def?) (ntelem (datum s.ast))]
                              [_ (wrong-syntax "expected terminal or non-terminal" #'s)])))
    (pattern t:non-symbol-token-name
             #:attr name #f
             #:attr mkast (lambda (lookup)
                            (telem (datum t.ast) #f))))

  (define-syntax-class symbol #:attributes (ast)
    (pattern x:id #:attr ast (syntax-e #'x)))

  (define-syntax-class token-name #:attributes (ast)
    (pattern (~or :symbol :non-symbol-token-name)))

  (define-syntax-class non-symbol-token-name #:attributes (ast)
    (pattern (~and x (~fail #:unless (let ([v (syntax-e #'x)])
                                       (or (char? v) (boolean? v) (exact-integer? v)))))
             #:attr ast (syntax-e #'x)))

  (define (make-auto-action nt index)
    #`(auto-action (quote #,nt) (quote #,index)))
  (define (make-proc-action proc)
    #`(#%expression #,proc))
  (define (make-expr-action venv expr)
    #`($lambda #,venv #,expr))

  (void))


;; ============================================================
;; Grammar syntax: non-terminal definitions

(module grammar-ct-syntax racket/base
  (require racket/base
           racket/syntax
           syntax/transformer
           syntax/free-vars
           syntax/parse
           (only-in syntax/datum datum)
           (submod ".." grammar-rep))
  (provide (all-defined-out))

  ;; ----------------------------------------

  (define-splicing-syntax-class grammar-clause
    (pattern (~seq #:import (~var gname (static grammar? "grammar")))
             #:attr p (lambda (gb)
                        (define impg (datum gname.value))
                        (gb-import-grammar gb impg (err:import-conflict #'gname))))
    (pattern (~seq #:value-terminals (t:symbol ...))
             #:attr p (lambda (gb)
                        (gb-add-terminals gb #t (datum (t.ast ...)) (datum (t ...))
                                          #:conflict err:already-declared)))
    (pattern (~seq #:empty-terminals (t:symbol ...))
             #:attr p (lambda (gb)
                        (gb-add-terminals gb #f (datum (t.ast ...)) (datum (t ...))
                                          #:conflict err:already-declared)))
    (pattern (~seq #:nonterminals (d:ntdef ...))
             #:attr p (lambda (gb)
                        (define lookup (gb->lookup gb))
                        (define defs (map (lambda (mk) (mk lookup)) (datum (d.mkast ...))))
                        (gb-add-nonterminals gb defs (datum (d.nt ...))
                                             #:conflict err:already-declared)))
    #;
    (pattern (~seq #:precedence (pd:precdecl ...))
             #:attr p (lambda (gb)
                        __))
    (pattern (~seq #:start t:symbol)
             #:attr p (lambda (gb) #;FIXME gb))
    (pattern (~seq #:end-terminals (t:symbol ...))
             #:attr p (lambda (gb) #;FIXME gb)))

  (define (err:already-declared name stx aux oldv newv)
    (wrong-syntax stx "already declared as ~a\n  symbol: ~e"
                  (match oldv
                    ['et "empty terminal"]
                    ['vt "value terminal"]
                    [(? def?) "nonterminal"])
                  name))

  (define ((err:import-conflict gname) name stx aux oldv newv)
    (err:already-declared name gname aux oldv newv))

  (void))


#|

;; ----------------------------------------

(begin-for-syntax
  (define (map-apply fs . xs) (for/list ([f (in-list fs)]) (apply f xs))))

(define-syntax define-grammar
  (syntax-parser
    [(_ name:id d:ntdef ...)
     (define (nt? s) (member s (datum (d.nt.ast ...))))
     (define-values (defs vals)
       (parameterize ((value-table (make-indexer)))
         (define defs (map-apply (datum (d.mkast ...)) nt?))
         (values defs (indexer->vector (value-table)))))
     (with-syntax ([(name-vals) (generate-temporaries
                                 (list (format "~a-vals-" (syntax-e #'name))))]
                   [(val ...) (vector->list vals)])
       #`(begin
           (define name-vals (vector-immutable val ...))
           (define-syntax name
             (grammar (quote #,defs) (quote-syntax name-vals)))))]))
|#
