#lang racket/base
(require "private/common.rkt"
         "private/syntax.rkt"
         "private/lr.rkt"
         "private/ll1.rkt")
(provide (all-from-out "private/common.rkt")
         define-grammar
         lr-parser
         ll1-parser
         filter:reject)

;; ----------------------------------------
;; TO DO

;; - better tokenizer support
;; - source locations, real token structure
;; - consistent conventions about absent payloads

;; - conflicts should be errors by default
;; - parser introspection (show states, dump debug table)
;;   - accessible at run time interactively (not like parser-tools/yacc debug)
;; - better parsing errors, hooks for users
