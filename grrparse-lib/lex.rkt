;; Copyright 2019-2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/contract/base
         "private/common.rkt"
         "private/lex-rx.rkt")

(define location-mode/c
  (or/c 'location
        'position
        'srcloc
        #f))

(define input-source/c
  (or/c input-port? bytes? string?))

(define lexer/c
  (-> input-source/c tokenizer?))

(define token-action/c
  (-> (or/c string? bytes?) any/c any/c
      (or/c token? (list/c token?) #f)))

(define token-reader/c
  (-> input-port? list? (values token? exact-nonnegative-integer?)))

(provide (contract-out
          #:unprotected-submodule unchecked
          [default-location-mode (parameter/c location-mode/c)]
          [make-lexer
           (->* [token-reader/c] [(hash/c symbol? token-reader/c)]
                lexer/c)]
          [peeking-lexer
           (-> lexer/c lexer/c)]
          [regexps-token-reader
           (->* []
                [#:location-mode location-mode/c
                 #:handle-eof? boolean?]
                #:rest (letrec ([c (or/c null?
                                         (cons/c (or/c regexp? byte-regexp?)
                                                 (cons/c token-action/c
                                                         (recursive-contract c))))])
                         c)
                token-reader/c)]
          [char-token-reader
           (->* [(listof char?)]
                [#:other-token-name symbol?
                 #:location-mode location-mode/c]
                token-reader/c)]
          [byte-token-reader
           (->* [(listof byte?)]
                [#:other-token-name symbol?
                 #:location-mode location-mode/c]
                token-reader/c)]
          ))
