;; Modified from parser-tools/lex

#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     parser-tools/private-lex/util
                     parser-tools/private-lex/actions
                     parser-tools/private-lex/front
                     parser-tools/private-lex/unicode-chars)
         (only-in parser-tools/private-lex/token position)
         racket/match
         racket/stxparam
         syntax/readerr)

(begin-for-syntax
  (define-syntax-class re-act-clause
    (pattern (re:expr act:expr)))

  (define (check-lexer trans start action-names stx)
    (when (vector-ref action-names start) ;; Start state is final
      (unless (and 
               ;; All the successor states are final
               (andmap (lambda (x) (vector-ref action-names (vector-ref x 2)))
                       (vector->list (vector-ref trans start)))
               ;; Each character has a successor state
               (let loop ([check 0] [nexts (vector->list (vector-ref trans start))])
                 (cond
                   [(null? nexts) #f]
                   [else (let ((next (car nexts)))
                           (and (= (vector-ref next 0) check)
                                (let ((next-check (vector-ref next 1)))
                                  (or (>= next-check max-char-num)
                                      (loop (add1 next-check) (cdr nexts))))))])))
        (raise-syntax-error #f "lexer can accept the empty string" stx)))))


(define-syntax peeking-lexer
  (syntax-parser
    [(_ (~alt ra:re-act-clause
              (~optional [#:eof eof-act])
              (~optional [#:special special-act])
              (~optional [#:special-comment special-comment-act]))
        ...)
     (define names (generate-temporaries #'(ra.act ...)))
     (define-values (trans start action-names no-look disappeared-uses)
       (build-lexer (map list (syntax->list #'(ra.re ...) names))))
     (check-lexer trans start action-names this-syntax)
     ;; ----
     (with-syntax ([start start]
                   [trans trans]
                   [no-lookahead no-look]
                   [(name ...) names]
                   [(act-name ...) (vector->list action-names)])
       (syntax-property
        (syntax/loc this-syntax
          (let ([name (wrap ra.act)] ...)
            (let ([proc
                   (lexer-body start 
                               trans
                               (vector act-name ...)
                               no-lookahead
                               (~? (wrap special-act) #f)
                               (~? (wrap special-comment-act) #f)
                               (~? (wrap eof-act) #f))])
              ;; reverse eta to get named procedures:
              (lambda (port) (proc port)))))
        'disappeared-use
        disappeared-uses))]))

(define-syntax wrap-action
  (syntax-parser
    [(_ action)
     (syntax/loc #'action
       (lambda (start-pos-p end-pos-p lexeme-p input-port-p)
         (syntax-parameterize 
             ([start-pos (make-rename-transformer #'start-pos-p)]
              [end-pos (make-rename-transformer #'end-pos-p)]
              [lexeme (make-rename-transformer #'lexeme-p)]
              [input-port (make-rename-transformer #'input-port-p)])
           (token-add-locations action start-pos-p end-pos-p))))]))

;; ----------------------------------------

(define ((lexer-body start-state trans-table actions no-lookahead
                     special-action special-comment-action eof-action)
         ip)
  (unless (input-port? ip) (raise-argument-error 'lexer "input-port?" 0 ip))
  (let lexer ()
    (define first-pos (get-position ip))
    (define first-char (peek-char-or-special ip 0))
    (define (lexer-loop state char longest-match-action
                        length-bytes length-chars longest-match-length)
      (define next-state
        (and (char? char)
             (get-next-state (char->integer char) (vector-ref trans-table state))))
      (cond [(not next-state)
             (check-match ip first-pos longest-match-length
                          length-chars longest-match-action)]
            [(vector-ref no-lookahead next-state)
             (define act (vector-ref actions next-state))
             (check-match ip 
                          first-pos 
                          (if act length-chars longest-match-length)
                          length-chars
                          (if act act longest-match-action))]
            [else
             (define act (vector-ref actions next-state))
             (define next-length-bytes (+ (char-utf-8-length char) length-bytes))
             (define next-char (peek-char-or-special ip next-length-bytes))
             (lexer-loop next-state 
                         next-char
                         (if act act longest-match-action)
                         next-length-bytes
                         (add1 length-chars)
                         (if act length-chars longest-match-length))]))
    ;; ----
    (cond [(eof-object? first-char)
           (do-match ip first-pos eof-action (read-char-or-special ip))]
          [(special-comment? first-char)
           (read-char-or-special ip)
           (if special-comment-action
               (do-match ip first-pos special-comment-action #f)
               (lexer))]
          [(not (char? first-char))
           (do-match ip first-pos special-action (read-char-or-special ip))]
          [else (lexer-loop start-state first-char (vector-ref actions start-state) 0 1 0)])))

(define (get-next-state char table)
  (cond [table
         (define (search min max)
           (if (>= min max)
               #f
               (let* ([try (quotient (+ min max) 2)]
                      [el (vector-ref table try)]
                      [r1 (vector-ref el 0)]
                      [r2 (vector-ref el 1)])
                 (cond [(and (>= char r1) (<= char r2)) (vector-ref el 2)]
                       [(< char r1) (search min try)]
                       [else (search (add1 try) max)]))))
         (search 0 (vector-length table))]
        [else #f]))

(define (check-match lb first-pos longest-match-length len longest-match-action)
  (unless longest-match-action
    (define m (read-string length lb))
    (match-define (position offset line col) first-pos)
    (raise-read-error (format "lexer: no match found in input starting with: ~e" m)
                      (object-name lb) line col offset len))
  (let ([m (read-string longest-match-length lb)])
    (do-match lb first-pos longest-match-action m)))

(define (do-match ip first-pos action value)
  (action first-pos (get-position ip) value ip))

(define (get-position ip)
  (let-values (((line col off) (port-next-location ip)))
    (position off line col)))
