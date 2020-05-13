#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/transformer
                     syntax/free-vars
                     (rename-in syntax/parse [attribute $])
                     "../util/datum-to-expr.rkt"
                     "../util/misc.rkt"
                     "grammar-rep.rkt")
         "common.rkt")
(provide (all-defined-out) (for-syntax (all-defined-out)))

;; In grammar specification, an Element is one of
;; - [name NT]
;; - [name T #:read (TR Expr ...)]
;; - [T #:top)]

;; In addition, the following abbreviated Elements are allowed:
;; - NT                         -- short for [_ NT]
;; - T                          -- short for [_ T #:read default]
;; - [name T]                   -- short for [name T #:read default]
;; - [name T #:read TR]         -- short for [name T #:read (TR)]
;; - [T #:read (TR Expr ...)]   -- short for [_ T #:read (TR Expr ...)]

(begin-for-syntax
  (define value-table (make-parameter #f)) ;; Indexer[Syntax]
  (define (add-value! stx) (indexer-add! (value-table) stx))

  (define params-spec (make-parameter null)) ;; (Listof Identifier)

  (define (map-apply fs . xs) (for/list ([f (in-list fs)]) (apply f xs)))
  (define (mk-$n n) (format-id (current-syntax-context) "$~a" n))

  (define-syntax-class ntdef #:attributes (nt nt.ast mkast)
    #:description "nonterminal definition"
    (pattern [nt:symbol ps:params-clause rhs:ntrhs ...]
             #:attr mkast (lambda (nt?)
                            (def ($ nt.ast) (length ($ ps.spec))
                              (parameterize ((params-spec ($ ps.spec)))
                                (for/list ([rhs-mkast (in-list ($ rhs.mkast))] [index (in-naturals)])
                                  (rhs-mkast ($ nt.ast) index nt?)))))))

  (define-splicing-syntax-class params-clause #:attributes (spec)
    #:description "context clause"
    (pattern (~seq #:context (param:id ...))
             #:attr spec (syntax->list #'(param ...)))
    (pattern (~seq)
             #:attr spec null))

  (define-syntax-class ntrhs #:attributes (mkast)
    (pattern [es:elemseq a:action]
             #:attr mkast (lambda (nt index nt?)
                            (define-values (es-ast venv) (($ es.mkast) nt?))
                            (define a-ast (($ a.mkast) nt index venv))
                            (prod nt index es-ast a-ast))))

  (define-splicing-syntax-class action #:attributes (mkast)
    #:description "action routine"
    (pattern (~seq #:auto ~!)
             #:attr mkast (lambda (nt index venv)
                            (add-value! #`(mk-auto-action (quote #,nt) (quote #,index)))))
    (pattern (~seq #:apply ~! proc:expr)
             #:attr mkast (lambda (nt index venv) (add-value! #'(mk-action proc))))
    (pattern (~seq (~optional (~seq #:> ~!)) e:expr ...+)
             #:attr mkast (lambda (nt index venv) (add-value! (wrap-expr #'(let () e ...) venv)))))

  (define-syntax-class elemseq #:attributes (mkast)
    #:description "element sequence"
    (pattern (e:elem ...)
             #:attr mkast (lambda (nt?)
                            ;; venv : (Listof Identifier), most recent (top of stack) first
                            (for/fold ([acc null] [venv null]
                                       #:result (values (list->vector (reverse acc)) venv))
                                      ([e-mkast (in-list ($ e.mkast))]
                                       [var (in-list ($ e.name))]
                                       [index (in-naturals 1)])
                              (define elem (e-mkast nt? venv))
                              (values (cons elem acc)
                                      (cond [(top-elem? elem) venv]
                                            [else (cons (or var (mk-$n index)) venv)]))))))

  (define-syntax-class elem #:attributes (name mkast)
    #:description #f
    (pattern :t/nt #:attr name #f)
    (pattern [:name :elem-content])
    (pattern [:elem-content] #:attr name #f)
    (pattern [t:token-name #:top] #:attr name #f
             #:attr mkast (lambda (nt? venv)
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (unless (pair? venv)
                              (wrong-syntax this-syntax
                                            "production cannot start with #:top element"))
                            (top-elem ($ t.ast)))))

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
    (pattern (~seq t:token-name #:read (tf:symbol arg:expr ...))
             #:with args:user-expr #'(list arg ...)
             #:attr mkast (lambda (nt? venv)
                            ;; FIXME: add tf and arity to list to check at runtime?
                            (when (nt? ($ t.ast)) (wrong-syntax #'t "expected terminal symbol"))
                            (telem ($ t.ast) (cons ($ tf.ast) (($ args.mkast) venv)))))
    (pattern (~seq :t/nt)))

  (define-syntax-class user-expr #:attributes (mkast)
    #:description "user expression"
    (pattern e:expr
             #:attr mkast (lambda (venv)
                            (define-values (fun refs)
                              (expand/make-user-expr #'e venv))
                            (expr:user (add-value! fun) refs))))

  (define-syntax-class symbol #:attributes (ast)
    (pattern x:id #:attr ast (syntax-e #'x)))

  (define-syntax-class token-name #:attributes (ast)
    (pattern (~or :symbol :non-symbol-token-name)))

  (define-syntax-class non-symbol-token-name #:attributes (ast)
    (pattern (~and x (~fail #:unless (let ([v (syntax-e #'x)])
                                       (or (char? v) (boolean? v) (exact-integer? v)))))
             #:attr ast (syntax-e #'x)))

  (define-splicing-syntax-class start-clause #:attributes (mkast)
    #:description "start clause"
    (pattern (~seq #:start start:symbol)
             #:attr mkast (lambda (nt?)
                            (unless (nt? ($ start.ast))
                              (wrong-syntax #'start "expected nonterminal"))
                            ($ start.ast))))

  (define-splicing-syntax-class end-clause
    #:description "end clause"
    (pattern (~seq #:end [t:token-name ...])
             #:attr mkast (lambda (nt?)
                            (for ([t (in-list (syntax->list #'(t ...)))])
                              (when (nt? (syntax-e t))
                                (wrong-syntax t "expected terminal")))
                            ($ t.ast)))
    (pattern (~seq #:implicit-end)
             #:attr mkast (lambda (nt?) #f)))

  (define-splicing-syntax-class grammar+start+end
    (pattern (~seq (~alt (~once (~seq #:grammar (~var g (static grammar? "grammar"))))
                         (~once start:start-clause)
                         (~optional end:end-clause))
                   ...)
             #:attr ast (let ()
                          (define nt? (grammar->nt? ($ g.value)))
                          (grammar+ ($ g.value)
                                    (($ start.mkast) nt?)
                                    ((or ($ end.mkast) (lambda (_) '(EOF))) nt?)))))

  (struct token-variable (vvar tvar)
    #:property prop:procedure
    (lambda (self stx)
      (let ([vvar (token-variable-vvar self)] [tvar (token-variable-tvar self)])
        ((make-variable-like-transformer #`(get-token-value '#,vvar #,tvar)) stx))))

  (define (expand/make-user-expr expr venv)
    (syntax-parse (local-expand (wrap-expr expr venv) 'expression null)
      #:literal-sets (kernel-literals)
      [(#%plain-lambda (tvar ...) body ...)
       (define tvars (syntax->list #'(tvar ...)))
       (define used-tvar-h
         (for/fold ([h (hasheq)])
                   ([var (in-list (free-vars #'(begin body ...)))])
           (cond [(member var tvars free-identifier=?)
                  => (lambda (vs) (hash-set h (car vs) #t))]
                 [else h])))
       (values
        (with-syntax ([(used-tvar ...)
                       (filter (lambda (v) (hash-ref used-tvar-h v #f)) tvars)])
          #'(#%plain-lambda (used-tvar ...) body ...))
        (for/list ([tvar (in-list (reverse tvars))] [i (in-naturals)]
                   #:when (hash-ref used-tvar-h tvar #f))
          i))]))

  (define (wrap-expr expr venv)
    (define rvenv (reverse venv)) ;; arguments are first-bound to last-bound
    (define tok-vars (generate-temporaries rvenv))
    (define-values (param-vars param-bindings)
      (let ([param-vars (generate-temporaries (params-spec))])
        (values param-vars
                (for/list ([param-var (in-list param-vars)]
                           [param (in-list (params-spec))])
                  #`[#,param (token-variable (quote-syntax #,param) (quote-syntax #,param-var))]))))
    (define tok-bindings
      (for/list ([tvar (in-list tok-vars)] [vvar (in-list rvenv)] #:when (identifier? vvar))
        #`[#,vvar (token-variable (quote-syntax #,vvar) (quote-syntax #,tvar))]))
    (with-syntax ([(tok-var ...) tok-vars]
                  [(tok-binding ...) tok-bindings]
                  [(param-var ...) param-vars]
                  [(param-binding ...) param-bindings]
                  [expr expr])
      #'(lambda (param-var ... tok-var ...)
          (letrec-syntax (param-binding ... tok-binding ...) expr))))

  (define (parse-grammar stx #:context ctx)
    (syntax-parse stx
      #:context ctx
      [(d:ntdef ...)
       (define (nt? s) (member s ($ d.nt.ast)))
       (parameterize ((value-table (make-indexer)))
         (define defs (map-apply ($ d.mkast) nt?))
         (grammar defs (indexer->vector (value-table))))]))

  (void))

(define (mk-action proc)
  (lambda toks (apply proc (map token-value* (filter token-with-value? toks)))))

(define (mk-auto-action nt index)
  (lambda toks (list* nt index (map token-value* (filter token-with-value? toks)))))

(define-syntax Grammar
  (syntax-parser
    [(_ part ...)
     (datum->expression (parse-grammar #'(part ...) #:context this-syntax)
                        (lambda (v) (cond [(syntax? v) v] [else #f])))]))

;; ----------------------------------------

(begin-for-syntax
  (define (parse-grammar* stx)
    (syntax-parse stx
      [(_ _ part ...)
       (parse-grammar #'(part ...) #:context stx)])))

(define-syntax define-grammar
  (syntax-parser
    [(_ name:id _ ...)
     #`(define-syntax name (parse-grammar* (quote-syntax #,this-syntax)))]))
