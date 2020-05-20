#lang scribble/manual
@(require scribble/basic)

@title[#:version "0.1"]{grrparse: Parsing Tools}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

This library provides support for lexing and parsing, similar to
@racketmodname[parser-tools/lex], @racketmodname[parser-tools/yacc], and
@racketmodname[parser-tools/cfg-parser], but with support for different features
and behavior.

@bold{Unstable} This library is a work in progress. @bold{This library's
interfaces and behavior may change in future versions.}

@include-section["intro.scrbl"]
@include-section["lex.scrbl"]
@;@include-section["parse.scrbl"]
