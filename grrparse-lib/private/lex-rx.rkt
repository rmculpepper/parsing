#lang racket/base
(require racket/match
         racket/list
         "common.rkt"
         "token.rkt")
(provide (all-defined-out))

(define default-location-mode (make-parameter 'location))

;; ============================================================
;; Lexers

;; Lexer = InputSource -> Tokenizer
;; InputSource = InputPort | String | Bytes

;; TokenReader = InputPort Args -> (values Token Nat)
;; The second result is the number of bytes peeked (but not read) to end of
;; token. A reader that reads instead of peeking should return 0 as second
;; result. Note: second result can be greater than token length, if previous
;; results were skipped.

;; make-lexer : TokenReader Hash[Symbol => TokenReader] -> Lexer
(define (make-lexer default-reader [readers #hasheq()])
  (define (lexer src) (make-tokenizer src default-reader readers))
  lexer)

;; source->input-port : InputSource -> InputPort
(define (source->input-port who src)
  (cond [(input-port? src) src]
        [(string? src) (let ([in (open-input-string src)]) (port-count-lines! in) in)]
        [(bytes? src) (let ([in (open-input-bytes src)]) (port-count-lines! in) in)]))

;; make-tokenizer : InputSource TokenReader Hash[Symbol => TokenReader] -> Tokenizer
(define (make-tokenizer src default-reader [readers #hasheq()])
  (define in (source->input-port 'make-tokenizer src))
  (define last-peek-amt 0)
  (define (get-token tf args)
    (commit-last)
    (define reader
      (cond [(eq? tf 'default) default-reader]
            [(hash-ref readers tf #f) => values]
            [else (error 'make-tokenizer:get-token "no reader\n  name: ~e" tf)]))
    (define-values (tok peek-amt) (reader in args))
    (set! last-peek-amt peek-amt)
    tok)
  (define (commit-last)
    (when (> last-peek-amt 0)
      (void (read-bytes last-peek-amt in))
      (set! last-peek-amt 0)))
  (tokenizer get-token commit-last))

;; ------------------------------------------------------------
;; Peeking lexer

(define ((peeking-lexer lx) src)
  (define in (source->input-port 'peeking-lexer src))
  (if (string-port? in)
      (rewinding-tokenizer lx in)
      (peeking-tokenizer lx in)))

(define (peeking-tokenizer lx in)
  (local-require (only-in racket/port peeking-input-port))
  (define peek-in (peeking-input-port in))
  (file-stream-buffer-mode peek-in 'none) ;; ?? might change in's mode ?!
  (port-count-lines! peek-in)
  (call-with-values (lambda () (port-next-location in))
                    (lambda vs (apply set-port-next-location! peek-in vs)))
  (match-define (tokenizer tz-get-token tz-commit-last) (lx peek-in))
  (define last-peek-amt 0)
  (define (get-token tf args)
    (commit-last)
    (tz-get-token tf args))
  (define (commit-last)
    (define saved-fpos (file-position peek-in))
    (tz-commit-last)
    (define diff (- (file-position peek-in) saved-fpos))
    (void (read-bytes diff in)))
  (tokenizer get-token commit-last))

(define (rewinding-tokenizer lx in)
  (match-define (tokenizer tz-get-token tz-commit-last) (lx in))
  (define last-state #f) ;; (list fpos line col pos) or #f
  (define (set-state state)
    (when state
      (file-position in (car state))
      (apply set-port-next-location! in (cdr state))))
  (define (get-state)
    (cons (file-position in)
          (call-with-values (lambda () (port-next-location in)) list)))
  (define (get-token tf args)
    (define saved-state (get-state))
    (set-state last-state)
    (begin0 (tz-get-token tf args)
      (set! last-state (get-state))
      (set-state saved-state)))
  (define (commit-last)
    (set-state last-state)
    (tz-commit-last)
    (set! last-state #f))
  (tokenizer get-token commit-last))

;; ============================================================
;; Regexps token reader

;; This uses the naive strategy of trying all regexps on every call to
;; get-token, rather than combining them into a single extended FA.

;; Advantages: reuses Racket's regexp support, including peek!
;; Disadvantages: can be slow if there are many regexps

;; Note: doesn't handle specials, because regexp-match et al raise error on
;; special.

;; Note: trying all regexps and selecting the longest match is *not* the same
;; as trying (re1|re2|...) and then disambiguating that lexeme, because eg
;;   (regexp-match #rx"ab|abc" "abc") = "ab"    -- not longest match !!

;; regexps-token-reader : { Regexp ActionFun }* -> TokenReader
(define (regexps-token-reader #:location-mode [locmode (default-location-mode)]
                              #:handle-eof? [handle-eof? #t]
                              . rx+action-list)
  (define table
    (make-lexer-table 'make-token-reader
                      (if handle-eof?
                          (list* #rx"^$" (lambda (l s e) 'EOF) rx+action-list)
                          rx+action-list)))
  (define (token-reader in [args null])
    (get-token table in locmode 0 #f))
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
       (cons (list->vector (reverse acc-rxs))
             (list->vector (reverse acc-actions)))])))

(define (convert-rx who rx)
  (define (convert-rep rep rebuild)
    (cond [(string? rep)
           (if (regexp-match? #rx"^\\^" rep) rx (rebuild (string-append "^" rep)))]
          [(bytes? rep)
           (if (regexp-match? #rx#"^\\^" rep) rx (rebuild (bytes-append #"^" rep)))]))
  (convert-rep (object-name rx)
               (cond [(pregexp? rx) pregexp]
                     [(regexp? rx) regexp]
                     [(byte-pregexp? rx) byte-pregexp]
                     [(byte-regexp? rx) byte-regexp]
                     [else (raise-argument-error who "regexp?" rx)])))

;; get-token : ... -> (values Token Nat)
(define (get-token table in locmode [peek?/skip #f] [start-loc #f])
  ;; If peek?/skip is #f, then read; if nat, peek with given skipped bytes.
  ;; Note: if peeking w/ skip, then start-loc should be correct for given skip.
  (define start (or start-loc (get-location locmode in)))
  (define skip (or peek?/skip 0))
  (define-values (index len-in-bytes) (peek-match (car table) in skip))
  (unless index (error 'get-token "failed\n  location: ~e" start))
  (define bytes-lexeme
    (cond [peek?/skip (peek-bytes len-in-bytes peek?/skip in)]
          [else (read-bytes len-in-bytes in)]))
  (define lexeme
    (cond [(byte-regexp? (vector-ref (car table) index)) bytes-lexeme]
          [else (bytes->string/utf-8 bytes-lexeme)]))
  (define end (get-end-location locmode in start len-in-bytes lexeme))
  (match ((vector-ref (cdr table) index) lexeme start end)
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

(define (peek-match rxv in [skip 0])
  (define-values (best-index best-end)
    (for/fold ([best-index #f] [best-end -1])
              ([index (in-naturals)] [rx (in-vector rxv)])
      (match (regexp-match-peek-positions rx in skip)
        [(cons (cons start end) _) ;; start = skip
         (if (> end best-end)
             (values index end)
             (values best-index best-end))]
        [#f
         (values best-index best-end)])))
  (values best-index (and best-index (- best-end skip))))

;; ============================================================
;; Location modes

;; StartLoc, EndLoc = Any -- depends on LocationMode

;; LocationMode =
;; | #f         -- always #f
;; | 'position  -- Nat, as reported by file-position
;; | 'location  -- location struct (below)
;; | 'srcloc    -- start is srcloc with span=#f, end is full srcloc
;; Note: for 'position and 'location, the end field describes one position after
;; the end of the token.

(struct location (line col pos) #:prefab)

;; get-location : LocationMode InputPort -> StartLoc
;; Returns a start location based on the input port's current position.
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

;; get-end-location : LocationMode InputPort StartLoc Nat (U String Bytes Char #f)
;;                 -> EndLoc
;; Predicts the end location based on start, length, and lexeme contents.
;; Note: cannot rely on the current port state, because peeking.
(define (get-end-location locmode in start len-in-bytes lexeme)
  (case locmode
    [(location)
     (match-define (location start-line start-col start-pos) start)
     (define-values (end-line end-col end-pos)
       (update-line-col-pos start-line start-col start-pos lexeme))
     (location end-line end-col end-pos)]
    [(position) ;; may be peeking, so can't use file-position
     (and len-in-bytes (+ start len-in-bytes))]
    [(srcloc)
     (match-define (srcloc src line col pos _) start)
     (define len-in-chars
       (cond [(string? lexeme) (string-length lexeme)]
             [(bytes? lexeme) (bytes-utf-8-length lexeme #\?)]
             [(char? lexeme) (char-utf-8-length lexeme)]
             [else #f]))
     (srcloc src line col pos len-in-chars)]
    [else #f]))

;; get-restart-location : LocationMode StartLoc EndLoc (U String Bytes Char #f)
;;                     -> StartLoc
;; Predicts the next start location after given (skipped) lexeme.
;; Note: cannot rely on the current port state, because peeking.
(define (get-restart-location locmode start end lexeme)
  (case locmode
    [(srcloc)
     (match-define (srcloc src line col pos _) start)
     (define-values (end-line end-col end-pos)
       (update-line-col-pos line col pos lexeme))
     (srcloc src end-line end-col end-pos #f)]
    [else end]))

;; update-line-col-pos : Nat/#f Nat/#f Nat/#f (U String Bytes Char #f)
;;                    -> (values Nat/#f Nat/#f Nat/#f)
(define (update-line-col-pos line col pos lexeme)
  ;; FIXME: "\t" not handled like Racket port-count-lines!
  (define (go newline-rx len get-col)
    (match (and (regexp-match? newline-rx lexeme)
                (regexp-match-positions* newline-rx lexeme))
      [#f (values line (and col (+ col len)) (and pos (+ pos len)))]
      [(? list? m)
       (values (and line (+ line (length m)))
               (and col (get-col (cdr (last m))))
               (and pos (+ pos len)))]))
  (cond [(string? lexeme)
         (define len (string-length lexeme))
         (go #rx"\r\n|\r|\n" len (lambda (last-nl) (- len last-nl)))]
        [(bytes? lexeme)
         (go #rx#"\r\n|\r|\n"
             (bytes-utf-8-length lexeme #\?)
             (lambda (last-nl) (bytes-utf-8-length lexeme #\? last-nl)))]
        [(char? lexeme)
         (define len (char-utf-8-length lexeme))
         (case lexeme
           ;; FIXME: This doesn't account for reading \r\n as separate chars
           [(#\newline #\return) (values (and line (add1 line)) 0 (and pos (add1 pos)))]
           [else (values line (and col (add1 col)) (and pos (add1 pos)))])]
        [else (values #f #f #f)]))

;; ============================================================
;; Token readers

(define (char-token-reader terminals
                           #:other-token-name [other-tname 'char]
                           #:location-mode [locmode (default-location-mode)])
  (peekX-token-reader terminals locmode peek-char char-utf-8-length values other-tname))

(define (byte-token-reader terminals
                           #:other-token-name [other-tname 'byte]
                           #:location-mode [locmode (default-location-mode)])
  (peekX-token-reader terminals locmode peek-byte (lambda (x) 1) integer->char other-tname))

(define (peekX-token-reader terminals locmode peek-x get-x-len get-x-lexeme other-t)
  (lambda (in args)
    (define start (get-location locmode in))
    (define next (peek-x in))
    (cond [(eof-object? next)
           (define end (get-end-location locmode in start 0 #f))
           (values (token/no-value 'EOF start end) 0)]
          [else
           (define len-in-bytes (get-x-len next))
           (define end (get-end-location locmode in start len-in-bytes (get-x-lexeme next)))
           (values (if (memv next terminals)
                       (token/no-value next start end)
                       (token other-t next start end))
                   len-in-bytes)])))
