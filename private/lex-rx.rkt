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

;; Note: trying all regexps and selecting the longest match is *not* the same
;; as trying (re1|re2|...) and then disambiguating that lexeme, because eg
;;   (regexp-match #rx"ab|abc" "abc") = "ab"    -- not longest match !!

;; ============================================================

;; TokenReader = InputPort Args -> (values Token Nat)
;; Results are token, number of bytes peeked (not read) to end of token.
;; A reader that reads instead of peeking should return 0 as second result.
;; Note: second result can be greater than token length, if previous results
;; were skipped.

;; make-token-reader : { Regexp ActionFun }* -> TokenReader
(define (make-token-reader #:location-mode [locmode 'location] . rx+action-list)
  (define table (make-lexer-table 'make-token-reader rx+action-list))
  (define (token-reader in [args null]) (get-token table in locmode 0 #f))
  token-reader)

;; LexerTable = (cons (Vectorof Regexp) (Vectorof ActionFun))
;; ActionFun = (String Loc Loc) -> ActionResult
;; ActionResult = Token         -- add locs if none present
;;              | (list Token)  -- return as-is (don't add locs)
;;              | #f            -- skip, read again (eg, for whitespace)

(define (make-lexer-table who args)
  (let loop ([args args] [acc-rxs null] [acc-actions null])
    (match args
      [(list* rx action args)
       (loop args
             (cons (convert-rx who rx) acc-rxs)
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

;; get-token : ... -> (values Token Nat)
(define (get-token table in locmode [peek?/skip #f] [start-loc #f])
  ;; If peek?/skip is #f, then read; if nat, peek with given skipped bytes.
  ;; Note: if peeking w/ skip, then start-loc should be correct for given skip.
  (define start (or start-loc (get-location locmode in)))
  (define skip (or peek?/skip 0))
  (define-values (action len-in-bytes) (table-peek-match table in skip))
  (unless action (error 'get-token "failed\n  location: ~e" start))
  (define lexeme
    (bytes->string/utf-8
     (cond [peek?/skip (peek-bytes len-in-bytes peek?/skip in)]
           [else (read-bytes len-in-bytes in)])))
  (define end (get-end-location+ locmode in start len-in-bytes lexeme))
  (match (action lexeme start end)
    [(? token? tok)
     (values (token-add-locations tok start end) (+ skip len-in-bytes))]
    [(list result) ;; return as is
     result]
    [#f ;; read again
     (get-token table in locmode
                (and peek?/skip (+ peek?/skip len-in-bytes))
                (and peek?/skip (get-restart-location locmode start end lexeme)))]
    [bad (error 'get-token "bad result from ~a action\n  result: ~e"
                'make-token-reader bad)]))

(define (table-peek-match table in [skip 0])
  (define-values (best-index best-end)
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
      (values (vector-ref (cdr table) best-index) (- best-end skip))
      (values #f #f)))

;; ----------------------------------------

;; LocationMode =
;; | #f         -- always #f
;; | 'position  -- Nat, as reported by file-position
;; | 'location  -- location struct (below)
;; | 'srcloc    -- start is srcloc with span=#f, end is full srcloc
;; Note: for 'position and 'location, the end field describes one position after
;; the end of the token.

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
    [else #f]))

(define newline-rx #rx"\r\n|\r|\n")

(define (get-end-location+ locmode in start len-in-bytes lexeme)
  (case locmode
    [(location)
     (match-define (location start-line start-col start-pos) start)
     (define-values (end-line end-col end-pos)
       (update-line-col-pos start-line start-col start-pos lexeme))
     (location end-line end-col end-pos)]
    [else (get-end-location locmode in start len-in-bytes (string-length lexeme))]))

(define (get-end-location locmode in start len-in-bytes len-in-chars)
  (case locmode
    [(position) ;; may be peeking, so can't use file-position
     (and len-in-bytes (+ start len-in-bytes))]
    [(srcloc)
     (match-define (srcloc src line col pos _) start)
     (srcloc src line col pos len-in-chars)]
    [else #f]))

(define (get-restart-location locmode start end lexeme)
  (case locmode
    [(srcloc)
     (match-define (srcloc src line col pos _) start)
     (define-values (end-line end-col end-pos)
       (update-line-col-pos line col pos lexeme))
     (srcloc src end-line end-col end-pos #f)]
    [else end]))

(define (update-line-col-pos line col pos lexeme)
  (define len (string-length lexeme))
  (match (and (regexp-match? newline-rx lexeme)
              (regexp-match-positions* #rx"\r\n|\r|\n" lexeme))
    [#f (values line (+m col len) (+m pos len))]
    [(? list? m) (values (+m line (length m)) (+m col (- len (cdr (last m)))) (+m pos len))]))

(define (+m x y) (and x (+ x y)))

;; ============================================================

(define (make-tokenizer in readers)
  (define default-reader (hash-ref readers 'default #f))
  (define last-peek-amt 0)
  (define (get-token tf args)
    (commit-last)
    (define reader
      (cond [(and (eq? tf 'default) default-reader) default-reader]
            [(hash-ref readers tf #f) => values]
            [else (error 'tokenizer:get-token "no reader\n  name: ~e" tf)]))
    (define-values (tok peek-amt) (reader in args))
    (set! last-peek-amt peek-amt)
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
   (lambda (lexeme start end) (token 'identifier lexeme))
   #rx"[0-9]+"
   (lambda (lexeme start end) (token 'number (string->number lexeme)))
   #rx"[ \t\n\r]+"
   (lambda _ '#f)
   #rx"\"(?:[^\"]|[\\\\][\"])*\""
   (lambda (lexeme start end) (token 'string lexeme))
   #rx"^$"
   (lambda _ 'EOF)))

(define (tokenizer-read-all tz)
  (match-define (tokenizer get-token commit-last _) tz)
  (let loop ()
    (eprintf ">>> ")
    (define tok (get-token 'default null))
    (eprintf "~e\n" tok)
    (cons tok (if (eq? (token-name tok) 'EOF) null (loop)))))

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
