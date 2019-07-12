#lang racket/base
(require racket/match
         "common.rkt"
         "lr-common.rkt")
(provide (all-defined-out))

(define (lr-parse states vals tz)
  (define DEBUG? #f)
  (define (get-token peek? tr stack)
    (cond [(symbol? (car tr))
           (tz peek? (car tr) (get-token-args (cdr tr) stack))]
          [(eq? (car tr) '#:apply)
           (apply->token (vector-ref vals (caddr tr)) (get-token-args (cdddr tr) stack))]
          [else (error 'lr-parse "bad tr: ~e" tr)]))
  (define (get-token-args args stack)
    (for/list ([arg (in-list args)])
      (match arg
        [(list datum) datum]
        [(? exact-nonnegative-integer? index)
         (tok-v (list-ref stack (+ index index -1)))])))

  (define (loop stack)
    (define st (vector-ref states (car stack)))
    (when DEBUG? (eprintf "\nSTATE = #~v, ~s\n" (car stack) (pstate-label st)))
    (cond [(pstate-accept st)
           => (lambda (accept)
                ;; Did we get here by a shift or a goto?
                (case accept
                  [(true) (tok-v (cadr (cddr stack)))]
                  [(virtual) (tok-v (cadr stack))]))]
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
    (define value (tok nt (apply (vector-ref vals action) args))) ;; (list* nt index args)
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
          [else (error 'lr-parse "next = ~v, state = ~v" next-tok (car stack))]))

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

(define (apply->token f args)
  (define v (apply f args))
  (list (if (token-name? v) v 'bad-token-name)))

(define (token-name? v)
  (or (symbol? v) (exact-integer? v) (boolean? v) (char? v)))
