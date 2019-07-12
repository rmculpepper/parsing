#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/id-table
                     syntax/transformer
                     (rename-in syntax/parse [attribute $])
                     "../util/datum-to-expr.rkt"
                     "grammar-rep.rkt")
         "common.rkt")
(provide (all-defined-out))

;; In grammar specification, an Element is one of
;; - [name NT]
;; - [name T #:read (TR Expr ...)]  -- where Expr = name | (quote Datum)
;; - [name T #:apply (RacketVariable Expr ...)]
;; where TExpr = name | (quote Datum).

;; In addition, the following abbreviated Elements are allowed:
;; - NT                             -- short for [_ NT]
;; - T                              -- short for [_ T #:read (default)]
;; - [name T]                       -- short for [name T #:read (default)]
;; - [name T #:read TR]             -- short for [name T #:read (TR)]
;; - [T #:read (TR TExpr ...)]      -- short for [_ T #:read (TR TExpr ...)]
;; - [T #:apply (RacketVar TExpr ...)] -- short for [_ name T #:apply (RacketVar TExpr ...)]

(begin-for-syntax
  (define intern-table (make-parameter #f)) ;; free-id-table[Id]
  (define value-table (make-parameter #f)) ;; Hash[Syntax => Nat]
  (define (add-value! stx)
    (let* ([h (value-table)] [index (hash-count h)]) (hash-ref! h stx index)))
  (define (intern-value! id)
    (add-value! (free-id-table-ref! (intern-table) id id)))
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
    (pattern (~seq (~optional #:>) ~! e:expr ...+)
             #:attr mkast (lambda (venv) (add-value! (wrap-expr #'(let () e ...) venv)))))
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
                                  [else (telem ($ s.ast) '(default))])))
    (pattern t:non-symbol-token-name
             #:attr name #f
             #:attr mkast (lambda (nt? venv) (telem ($ t.ast) '(default)))))
  (define-splicing-syntax-class elem-content #:attributes (mkast)
    #:description "element content"
    (pattern (~seq t:token-name #:read tk:symbol)
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tk to list to check at runtime?
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (telem ($ t.ast) (list ($ tk.ast)))))
    (pattern (~seq t:token-name #:read (tf:symbol arg:texpr ...))
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tf and arity to list to check at runtime?
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (telem ($ t.ast) (cons ($ tf.ast) (map-apply ($ arg.mkast) venv)))))
    (pattern (~seq t:token-name #:apply (re:id arg:texpr ...))
             #:attr mkast (lambda (nt? venv)
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (record-disappeared-uses #'re)
                            (let* ([re (free-id-table-ref! (intern-table) #'re #'re)]
                                   [re-index (add-value! re)])
                              (telem ($ t.ast)
                                     (list* '#:apply (syntax-e re) re-index
                                            (map-apply ($ arg.mkast) venv))))))
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
             #:attr mkast (lambda (venv) (list (syntax->datum #'datum)))))
  (define-syntax-class symbol #:attributes (ast)
    (pattern x:id #:attr ast (syntax-e #'x)))
  (define-syntax-class token-name #:attributes (ast)
    (pattern (~or :symbol :non-symbol-token-name)))
  (define-syntax-class non-symbol-token-name #:attributes (ast)
    (pattern (~and x (~fail #:unless (let ([v (syntax-e #'x)])
                                       (or (char? v) (boolean? v) (exact-integer? v)))))
             #:attr ast (syntax-e #'x)))

  (define (index-hash->vector h)
    (define v (make-vector (hash-count h)))
    (for ([(e i) (in-hash h)]) (vector-set! v i e))
    v)

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
     (parameterize ((intern-table (make-free-id-table)) (value-table (make-hash)))
       (define defs (map-apply ($ d.mkast) nt?))
       (datum->expression (grammar ($ start.ast) defs (index-hash->vector (value-table)))
                          (lambda (v) (cond [(syntax? v) v] [else #f]))))]))
