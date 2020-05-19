#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "token.rkt")
(provide (all-defined-out))

;; This uses the naive strategy of trying all regexps on every call to
;; get-token, rather than combining them into a single extended FA.

;; Advantages: reuses Racket's regexp support, including peek!
;; Disadvantages: can be slow if there are many regexps

;; Note: trying all regexps and selecting the longest match is *not* the same as
;; trying (re1|re2|...) and then disambiguating that lexeme, because eg
;;   (regexp-match #rx"ab|abc" "abc") = "ab"    !! not longest match !!

;; make-token-reader : { Regexp (?? -> Token) }* -> TokenReader
(define (make-token-reader #:location-mode [loctype 'location]
                           . rx+action-list)
  (define table (make-lexer-table rx+action-list))
  (define (token-reader in [args null])
    (let loop ([skip 0])
      (define-values (t len) (get-token table in loctype #f skip))
      (if t (values t len) (loop (+ skip len)))))
  token-reader)

;; LexerTable = (cons (Vectorof Regexp) (Vectorof (String Loc Loc -> Token)))

(define (make-lexer-table args)
  (let loop ([args args] [acc-rxs null] [acc-actions null])
    (match args
      [(list* rx action args)
       (loop args
             (cons (convert-rx 'make-lexer-table rx) acc-rxs)
             (cons action acc-actions))]
      ['()
       (cons (list->vector (reverse acc-rxs)) (list->vector (reverse acc-actions)))])))

(define (convert-rx who rx)
  (define (convert-rep rep k)
    (cond [(string? rep)
           (if (regexp-match? #rx"^\\^" rep) rx (k (string-append "^" rep)))]
          [(bytes? rep)
           (if (regexp-match? #rx#"^\\^" rep) rx (k (bytes-append #"^" rep)))]))
  (cond [(pregexp? rx) (convert-rep (object-name rx) pregexp)]
        [(regexp? rx) (convert-rep (object-name rx) regexp)]
        [else (raise-argument-error who "regexp?" rx)]))

;; ----

;; get-token : ... -> (values Token/#f Nat)
(define (get-token table in loctype [start-loc #f] [peek?/skip #f])
  ;; If peek?/skip is #f, then read; if nat, peek with given skipped bytes.
  ;; Note: if peeking w/ skip, then start-loc should be correct for given skip.
  (define start (or start-loc (get-location loctype in)))
  (define-values (action len-in-bytes) (table-peek-match table in (or peek?/skip 0)))
  (unless action (error 'get-token "failed\n  location: ~e" start))
  (define lexeme
    (bytes->string/utf-8
     (cond [peek?/skip (peek-bytes len-in-bytes peek?/skip in)]
           [else (read-bytes len-in-bytes in)])))
  (define end (get-end-location+ loctype in start len-in-bytes lexeme))
  (values (action lexeme start end) len-in-bytes))

(define (table-peek-match table in [skip 0])
  (define-values (best-index best-len-in-bytes)
    (for/fold ([best-index #f] [best-end -1])
              ([index (in-naturals)] [rx (in-vector (car table))])
      (match (regexp-match-peek-positions rx in skip)
        [(cons (cons start end) _) ;; start = skip
         (if (> end best-end)
             (values index end)
             (values best-index best-end))]
        [#f
         (values best-index best-end)])))
  (if best-index
      (values (vector-ref (cdr table) best-index) best-len-in-bytes)
      (values #f #f)))

;; ----

;; LocationMode =
;; | #f         -- always #f
;; | 'position  -- Nat, as reported by file-position
;; | 'location  -- location struct (below)
;; | 'srcloc    -- start is srcloc with span=#f, end is full srcloc

(struct location (line col pos) #:prefab)

(define (get-location locmode in)
  (case locmode
    [(position) (file-position in)]
    [(location)
     (define-values (line col pos) (port-next-location in))
     (location line col pos)]
    [(srcloc)
     (define-values (line col pos) (port-next-location in))
     (srcloc (object-name in) line col pos #f)]
    [else #| (#f) |# #f]))

(define (get-end-location+ locmode in start len-in-bytes lexeme)
  (define newline-rx #rx"\r\n|\r|\n")
  (case locmode
    [(location)
     (define len (string-length lexeme))
     (match-define (location start-line start-col start-pos) start)
     (match (and (regexp-match? newline-rx lexeme)
                 (regexp-match-positions* #rx"\r\n|\r|\n" lexeme))
       [#f
        (location start-line (+ start-col len) (+ start-pos len))]
       [(? list? m)
        (location (+ start-line (length m))
                  (+ start-col (- len (cdr (last m))))
                  (+ start-pos len))])]
    [else (get-end-location locmode in start len-in-bytes (string-length lexeme))]))

(define (get-end-location locmode in start len-in-bytes len-in-chars)
  (case locmode
    [(position) ;; may be peeking, so can't use file-position
     (and len-in-bytes (+ start len-in-bytes))]
    [(srcloc)
     (match-define (srcloc src line col pos _) start)
     (srcloc src line col pos len-in-chars)]
    [else #f]))

;; ----------------------------------------

;; TokenReader = InputPort Args -> (values Token Nat)
;; Results are token, token length in bytes peeked but not read.
;; A reader that reads instead of peeking should return 0 as second result.

#;
;; token-reader-add-locations : TokenReder LocationMode -> TokenReader
(define ((token-reader-add-locations read-token [locmode 'location]) in args)
  (define start (get-location locmode in))
  (define-values (tok len-in-bytes) (read-token in args))
  (define end (get-end-location locmode in start len-in-bytes #f))
  (values (token-update-locations tok start end) len-in-bytes))

(define (make-tokenizer in readers)
  (define default-reader (hash-ref readers 'default #f))
  (define last-peek-amt 0)
  (define (get-token tf args)
    (commit-last)
    (define reader
      (cond [(and (eq? tf 'default) default-reader) default-reader]
            [(hash-ref readers tf #f) => values]
            [else (error 'tokenizer:get-token "no reader\n  name: ~e" tf)]))
    (define-values (tok len) (reader in args))
    (set! last-peek-amt len)
    tok)
  (define (commit-last)
    (when (> last-peek-amt 0)
      (void (read-bytes last-peek-amt in))
      (set! last-peek-amt 0)))
  (tokenizer get-token commit-last #f))


;; XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

(define tr
  (make-token-reader
   #:location-mode 'location
   #rx"[a-zA-Z][a-zA-Z0-9]*"
   (lambda (lexeme start end) (token 'identifier lexeme start end))
   #rx"[0-9]+"
   (lambda (lexeme start end) (token 'number (string->number lexeme) start end))
   #rx"[ \t\n\r]+"
   (lambda _ '#f)
   #rx"\"(?:[^\"]|[\\\\][\"])*\""
   (lambda (lexeme start end) (token 'string lexeme start end))
   #rx"^$"
   (lambda _ 'EOF)))

(define (read-all-tokens tr s)
  (define in (open-input-string s))
  (port-count-lines! in)
  (let loop ()
    (eprintf ">>> ")
    (define-values (tok nbytes) (tr in null))
    (eprintf "~e\n" tok)
    (void (read-bytes nbytes in))
    (cons tok (if (eq? (token-name tok) 'EOF) null (loop)))))

(define s #<<EOF
abc 23 0 i18n "hello world!"
EOF
)
