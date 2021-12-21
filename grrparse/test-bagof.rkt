#lang racket/base
(require racket/class
         racket/list
         racket/struct
         racket/match
         grrparse
         grrparse/util)
(provide (all-defined-out))

(define PRINT? #f)

(module util racket/base
  (require racket/match grrparse)
  (provide (all-defined-out))

  (define (make-toks vs)
    (for/list ([v (in-list vs)] [i (in-naturals)])
      (match v [(list t) (token t t i i)] [(list t v) (token t v i i)])))

  (define (mktz vs)
    (define toks (make-toks vs))
    (define (get-token _k _a)
      (if (pair? toks) (begin0 (car toks) (set! toks (cdr toks))) (token 'EOF)))
    (tokenizer get-token void)))

(require 'util)

;; ============================================================

;; Example with LR0 (and LR1?) shift/reduce conflict
(define-grammar g5
  [E [(a) #:> $1]
     [(E op E) #:> (list $1 $2 $3)]
     [(MkBox lp E rp) #:> (put-result $1 $3)]]
  [MkBox [() #:> (parse-box null)]])

(define gg5 (lr-parser #:grammar g5 #:start E))

(define s5a '((lp) (a) (op) (a) (op) (a) (rp) (op) (a)))
(define parses (send gg5 parse* (mktz s5a)))
(collect-parses parses)

;; ============================================================

(define-grammar gx
  [MkBox [() #:> (parse-box null)]]
  [E [(a op a) #:apply list]
     [(a) $1]]
  [S [([b1 MkBox] [e1 E] [o op] [b2 MkBox] [e2 E])
      (list (put-result b1 e1) o (put-result b2 e2))]])

(define ggx (lr-parser #:grammar gx #:start S))

(define sxa (map list '(a op a op a)))
(collect-parses (send ggx parse* (mktz sxa)))
;; Problem! parse-box gets all parses starting at a point, even if
;; they end at different points. So can get nonsense results, like
;; (#<parse-box: a (a op a)> op #<parse-box: (a op a)>), which implies
;; a 4-atom/3-op sentence!

;; If action routine had access to token counter, could have StartBox
;; NT return a memoized Counter -> Box function....

;; ============================================================

;; alt version of g5
(define-grammar gq
  [E [(a) #:> $1]
     [(E op E) #:> (list $1 $2 $3)]
     [(lp E rp) #:> (action:collect $2)]])

(define ggq (lr-parser #:grammar gq #:start E))

(define sqa '((lp) (a) (op) (a) (op) (a) (rp) (op) (a)))
(send ggq parse* (mktz sqa))

;; ============================================================

;; alt version of gx
(define-grammar gz
  [E [(a op a) #:apply list]
     [(a) $1]]
  [E* [(E) (action:collect $1)]]
  [S [([e1 E*] [o op] [e2 E*])
      (list e1 o e2)]])

(define ggz (lr-parser #:grammar gz #:start S))

(define sza (map list '(a op a op a)))
(send ggz parse* (mktz sza))
