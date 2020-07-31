# Design

This page describes some non-standard features I want, and how to fit them
within the LR-automaton implementation.

## Multiple token-readers

That is, I want to be able to determine how tokens are read based on the parser
state. For example:

  S ::= [hasString read-prefix] [data read-string]
      | [hasInteger read-prefix] [data read-number]

(where hasString and hasInteger are tokens readable via read-prefix, and data is
a token readable by read-string or read-number --- same name, different readers,
different payloads)

Somewhat related literature: PSLR(1) (pseudo-scannerless).

Assuming that the input must be read linearly, that means that at every parsing
state there must be a single token reader used to make the next transition. For
example, the following grammar is bad:

  S ::= [a default] [b read-1]
      | [a default] [c read-2]

because after reading 'a' with the default reader, there are still two possible
paths, but they require different readers to get and check the next token. For
deterministic LR, it's possible to detect *inconsistent reader* problems
statically; for GLR, I'm not sure if static checking is tractable (it may be too
conservative), so might be best to do it dynamically.

Implementation: Each terminal element in a production has an associated
token-reader field. When constructing an LR0 state's shift and reduce tables,
check that there is a single token reader consistent with all shift transitions
and reduce lookaheads for that state. For a GLR parser, we must compute
lookaheads for token-reader consistency check even if the automaton won't use
them to resolve S/R and R/R conflicts.

## Parameterized terminals

That is, extend the idea of multiple token readers to allow readers to take
parameters, computed from earlier parsed values. For example:

  S ::= [n : stringLength read-prefix] [data read-string(n)]

This makes the consistency problem harder, because in the general case it would
require deciding whether two Racket expressions are equivalent (impossible). So
we must use a conservative approximation.

I expect (hope), given languages designed to be parsed unambiguously, that
parameterized token readers will be used after a disambiguating prefix, so they
won't appear together in states with other items anyway.

Implementation: Add a variant of token-reader with arguments. The arguments are
represented by a "user expression" in essentially the same way as an action
routine.

## Digression: Review of LR Automata and their Stacks

The following sections require some knowledge of how LR automata work and in
particular how the stack works. Here's a review of some of the important details.

The stack of an LR-automaton consists of alternating State and Value slots, both
starting and ending with a State slot: That is (top = left):

  stack = [stateN+1 valueN stateN ... value0 state0]

Each [valueK stateK] segment consists of a terminal- or nonterminal-tagged value
pushed by stateK, where the terminal or nonterminal appears after the dot in one
or more of the states items. For example, if stateK contains an item of the form
"NT → α • t β", and the parser reads a "t" token, then it pushes the value
containing that tag "t" and the token's payload as valueK, and then it pushes
and enters the appropriate state containing the item "NT → α t • β".

The stack is popped when the top state (sN+1) contains an item with the bullet
at the end: "NT → γ •" (subject to lookahead), and control returns to the state
at the top of the stack after popping the appropriate number of frames from the
stack --- and sN+1 determines how much to pop, not its "caller". In general, any
state can either be a return point, or it can be skipped over entirely by a
return to an earlier state.

For example, consider the following grammar:

  S  ::= AB c | a BD
  AB ::= a b
  BD ::= b d

Here are some of the states and their transitions:

  state0:               state1:                 state2:                 state3:
    S → • AB c    –a→                    –b→                     –d→     
    S → • a BD            S → a • BD
    AB → • a b            AB → a • b              AB → a b •              
                          BD → • b d              BD → b • d              BD → b d •

After reading "ab", the stack looks like this:

  [state2 "b" state1 "a" state0]

If the lookahead token is a "c", the automaton reduces the AB item in state2,
popping two values and returning it to state0 --- jumping over state1
completely!

  [AB state0]   -- on return, stack temporarily has value at top, not state

Then state0 goes to a state (not shown) containing "S → AB • c".

If the lookahead token is a "d", the automaton shifts it onto the stack and
moves to state3:

  [state3 "d" state2 "b" state1 "a" state0]

and then state3 reduces BD and returns to state1 --- jumping over state2!

  [BD state1 "a" state0]

Then state1 goes to a state (not shown) containing "S → a BD •".

The point is that neither state1 nor state2 know whether they'll be returned to
or skipped. It is also true (but not demonstrated by this example) that the
popping state doesn't necessarily (statically) know what state it will return to
--- that information is on the stack. That makes changes to the stack layout
complicated or impossible. For example, one could imagine an optimization that
deleted states from the stack if they are not potential return points (because
they have no item with a nonterminal after the dot). But now all states that
might jump past that point must be updated to jump correctly --- and it's
possible that sometimes they are also reachable via other paths that should
*not* be adjusted. So, care must be taken.

## Parameterized nonterminals

Nonterminals are the abstraction mechanism for context-free grammars, so if
terminals can have parameters then nonterminals should be able to have
parameters too. For example, it should be possible to refactor the example above
into the following:

  S ::= [n : stringLength read-prefix] StringData(n)
  StringData(n) ::= [data read-string(n)]

How do we pass and receive parameters within the table-driven LR automaton
framework? What happens if a parameterized nonterminal is used in parallel with
a non-parameterized nonterminal

  S ::= A(1) | B
  A(n) ::= a
  B ::= b

or if multiple parameterized nonterminals are used in parallel

  S ::= A(1) | B(2)
  A(n) ::= a
  B(n) ::= b

or perhaps the same nonterminal with different parameters

  S ::= A(1) | A(2)
  A(n) ::= a

In these examples, the parameters don't affect parsing, but could be used in the
action routine, which I've omitted.

There are two basic groups of possible implementations: parameters are stored on
the stack "specially", or the parameters are stored on the stack as values.

### Parameters as special slots

By storing parameters specially, I mean this stack layout:

  [stateN+1 paramsN+1 valueN stateN paramsN ... value0 state0 params0]

The paramsK slot stores the parameters of nonterminals that appear after dots in
stateK. By changing the stack structure in a consistent way, we avoid the
problems mentioned at the end of the digression above.

Exactly how do we store the parameters in that slot?

Consider the "S ::= A(1) | B" example above. If we store 1 directly in the
parameters slot for the "S → • A, S → • B, ..." state, then A knows where to
find its parameters, and B doesn't have any so there's no conflict.

But consider the "S ::= A(1) | B(2)" example. We have one state representing the
beginnings of both A and B; we must keep their parameters separate. We could
store a dictionary mapping "A" to 1 and "B" to 2.

Now consider the "S ::= A(1) | A(2)" example. We cannot disambiguate the
parameters by the name of the nonterminal they belong to. For a deterministic LR
parser, it's not clear that this example should be allowed at all: we're asking
it to merge explorations that are fundamentally the same, given the structure
visible to the LR table construction. For a nondeterministic LR parser, it is
fine in principle to explore both paths simultaneously, but we would have to
detect this kind of conflict and add a new "fork" operation for this
purpose. Complicated.

### Parameters as value slots

An alternative is to add a simpler feature: out-of-bounds reads! That is, an
expression in a production is allowed to read from further down the stack than
the beginning of the enclosing nonterminal. That is, it can read from the
nonterminal's context. This feature enables the following strategy:

Parameterized nonterminals expect their parameters to be placed in the value
slot immediately before the state that contains the beginning of the
nonterminal. Uses of parameterized nonterminals use auxiliary empty nonterminals
to "load" the parameters before "calling" the parameterized nonterminal. For
example, this grammar (writing ▶ before the action routine and $[n] for the nth
value on the stack, indexed from 1):

  S ::= A(1)
  A(n) ::= a            ▶ n + $[1]

is desugared to this one:

  S ::= LoadParams A
  LoadParams ::= ε      ▶ 1
  A ::= a               ▶ $[2] + $[1]   -- note that $[2] is "out of bounds"

The addition of null nonterminals like LoadParams can lead to Shift/Reduce and
Reduce/Reduce conflicts. Some of those conflicts can be eliminated by lookahead;
others cannot. But the "S ::= A(1) | A(2)" example is now *only* problematic
because of a Reduce/Reduce conflict, and a GLR parser can handle it by forking
its exploration in the same way it handles any conflict unresolved by lookahead.

If the indexes of stack reads in user expressions are statically fixed and
known, a simple static analysis can determine whether the grammar is safe ---
that is, it never reads past the bottom of the stack. (FIXME: implement)

## Disambiguation filters

"Disambiguation filters" are used with GLR parsers to prune and prioritize the
potentially multiple parse results. A basic but useful kind of filter is the
rejection filter: the ability to mark a production as invalid.

Implementation: The parser library provides a reject structure, and if an action
routine returns a reject instance, that thread of exploration is discarded.

## Branching based on values

It is possible to encode deterministic choice based on computed values using
existing features: first make a nondeterminstic choice and then use a rejection
filter to eliminate the undesired alternatives. This only works in a GLR
(nondeterministic) parser, though.

In order to support deterministic choice, the parser supports a new kinds of
element in productions: [T #:top] inspects the top *value* on the stack and
matches the payload (not the tag!) against the terminal T. The expression that
controls the path chosen is loaded onto the stack using a null nonterminal,
similar to a nonterminal parameter.

FIXME: consistency

## Error reporting

There is an interface to allow users to inspect (a projection of) the stack when
a parse error occurs. Together with automatic source location tracking, this
makes it somewhat easier to understate the state of the parser when the error
occurred --- even without looking at state and transition tables. For example,
consider the following grammar:

  Sent   ::= Clause
           | Clause conj Sent
  Clause ::= NP VP
  NP     ::= noun
           | adj NP
  VP     ::= verb Obj
           | adv VP
  Obj    ::= ε
           | NP

The parser errors on the input "noun adv verb conj adj noun noun". We can extract the stack,
discard the states, and reverse it to get the following (with token contents
omitted):

  #s(token Clause _ 0 2) #s(token conj _ 3 3) #s(token NP _ 4 5) #(token noun _ 6 6)

That is, tokens 0 to 2 were parsed into a Clause, token 3 is a conjunction,
tokens 4 to 5 were parsed as a Noun Phrase, and token 6 caused the error.

The states can be left it, and that's more informative, but it also requires
some understanding of LR automata states.

## When to accept?

Consider the following language:

  E ::= ( Term )
  Term ::= Term op Term | E | atom

The language is ambiguous, but that's not the point. The point is that it is
possible to read and parse an E without inspecting the input stream past the
final ")". There may be multiple parses of the E, but the endpoint is
unambiguous. The same is *not* true of Term; a valid Term can always be
extended; one can only declare a Term "done" if the lookahead token is not
"op".

The traditional description of LR parsing has a pre-processing step which adds a
new start symbol and a special end-of-input marker. If the original start symbol
were Term, that yields the following:

  S ::= Term EOI
  Term ::= Term op Term | E | atom
  E ::= ( Term )

Then in the state containing "Term → Term op Term •", if the lookahead token is
"op", we shift and continue parsing; if it is ")" or "EOI" then we reduce.

Another approach (taken by Racket's parse-tools library) is to specify a set of
tokens that can follow a successful parse. I believe it is sufficient to put
that set as the start symbol's follow set; then SLR(1) or LALR(1) lookahead
computation will use those symbols to disambiguate (or signal conflicts, if they
do not completely disambiguate).

The follow-tokens approach still implies that the parser reads one token beyond
the end of the input, however. There are two modes that one might want a parser
to operate in:

- externally-delimited : Parsing stops when the tokenizer encounters the end of
  input (or when the parser reduces the start symbol with a lookahead token in
  the follows set). Example: Term above. The traditional implementation should
  be sufficient.

- internally-delimited : Parsing stops only when the parser reduces the start
  symbol *without* looking at the following token. Example: E above.

Implementation: Add "S' → S EOI" (where S' is fresh, S is the old start symbol)
and compute the LR states as usual. If EOI appears in the reduction lookahead
table, and either (1) the shift table is non-empty or (2) some other terminal
appears in the lookahead table, then signal an error: the grammar is not
internally-delimited. Otherwise, omit any lookahead table containing EOI, so
that state will reduce without lookahead, and mark the state containing
"S' → S • EOI" as an accept state.

Caveat: The internally-delimited property ensures that the *parser* does not
read any *tokens* past the sentence produced by the start symbol. The tokenizer,
however, may need to read bytes past the end of the last token as part of
lexical analysis. That's a different but related property. One can combine them
by computing the set Last(S) --- the set of possible final terminals in an
S-sentence, dual to First(S) --- and checking if those tokens have a generalized
version of the prefix property. That is: let T = Strings(Last(S)) and let L =
Strings(Terminals(S)); then if s ∈ T then there is no t such that concat(s,t) ∈ L.

## Tokenizer interface

- read-token commits last read, peeks to get next token
- commit-last - only called once, at end of parsing ?
