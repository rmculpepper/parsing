#lang scribble/manual
@(require scribble/basic
          scribble/example
          (for-label racket/base
                     racket/contract
                     racket/match
                     grrparse grrparse/lex))

@(define the-eval (make-base-eval))

@title[#:tag "intro"]{Introduction to grrparse}

First, we require the library, including lexer support:

@examples[#:eval the-eval #:label #f
(require grrparse grrparse/lex)
]

First we'll define a lexer for a simple arithmetic language. In
@racketmodname[grrparse], a @emph{lexer} consists of one or more @emph{token
readers}. This example uses only one. We use @racket[regexps-token-reader] to
create a token reader given a list of alternating regexps and action routines:

@examples[#:eval the-eval #:label #f
(define arith-lex
  (make-lexer
   (regexps-token-reader
    #rx"\\("    (lambda (l s e) 'lparen)
    #rx"\\)"    (lambda (l s e) 'rparen)
    #rx"[0-9]+" (lambda (l s e) (token 'num (string->number l)))
    #rx"[+]"    (lambda (l s e) '+)
    #rx"[*]"    (lambda (l s e) '*)
    #px"\\s+"   (lambda (l s e) #f))))
]

Each action routine gets the @emph{lexeme} matched by the preceding regular
expression and the start and end locations. An action routine typically returns
a token, consisting of a terminal name and possibly a value. A ``safe'' terminal
name (a symbol, character, or exact integer) is considered a token with no value
and no locations. If the action routine returns a token without locations, the
start and end locations are added automatically.

If the action routine returns @racket[#f], the match is skipped and the reader
is called again. This behavior is useful for whitespace (as above) and comments.

The @racket[regexps-token-reader] function automatically adds a handler for the
end of input, returning an empty token with the terminal @racket['EOF].

Applying a lexer to an input source produces a @emph{tokenizer}. The following
example constructs a tokenizer from a string and reads tokens from it until an
@racket['EOF] token is returned:

@examples[#:eval the-eval #:label #f
(let ([tz (arith-lex "(1 +\n 2*3)")])
  (let loop ()
    (define next ((tokenizer-get-token tz) 'default null))
    (cons next (case (token-name next)
                 [(EOF) null]
                 [else (loop)]))))
]

A grammar consists of a sequence of nonterminal definitions. Each nonterminal
has one or more right-hand sides, and a right-hand side contains an
@emph{element sequence} and an @emph{action expression}.

Here is a grammar for arithmetic expressions:

@examples[#:eval the-eval #:label #f
(define-grammar arith-g
  [T [(F + T)           (list '+ $1 $3)]
     [(F)               $1]]
  [F [([x S] * [y F])   (list '* x y)]
     [([x S])           x]]
  [S [([n num])         n]
     [(lparen T rparen) $2]])
]

An element consists of an optional variable and a terminal or nonterminal
reference. For example, in the first right-hand side for @racket[T] above, the
first element, @racket[F], is a nonterminal reference without a variable, so the
variable @racket[$1] is implicitly added. In the first right-hand side for
@racket[F], the first element, @racket[[x S]], consists of the variable
@racket[x] and the nonterminal @racket[S].

Now we can create a @emph{parser} from the grammar:

@examples[#:eval the-eval #:label #f
(define arith-parser
  (lr-parser #:grammar arith-g #:start T #:end (EOF)))
]



@examples[#:eval the-eval #:label #f
(require racket/class)
(send arith-parser parse (arith-lex "(1 + 2 * 2)"))
(send arith-parser parse (arith-lex "4 * (5 + 6) + 7"))
(send arith-parser parse (arith-lex "8 + 9 + 10"))
]

@examples[#:eval the-eval #:label #f
(eval:error (send arith-parser parse (arith-lex "(1 + ")))
]




@examples[#:eval the-eval #:label #f
(define-grammar ambi-arith-g
  [T [(T + T)           (list '+ $1 $3)]
     [(F)               $1]]
  [F [([x F] * [y F])   (list '* x y)]
     [([n num])         n]
     [(lparen T rparen) $2]])
]

@examples[#:eval the-eval #:label #f
(define ambi-arith-parser
  (lr-parser #:grammar ambi-arith-g #:start T #:end (EOF)))
]

@examples[#:eval the-eval #:label #f
(send arith-parser parse* (arith-lex "(1 + 2 * 2)"))
(send arith-parser parse* (arith-lex "4 * (5 + 6) + 7"))
(send arith-parser parse* (arith-lex "8 + 9 + 10"))
]

@examples[#:eval the-eval #:label #f
]
