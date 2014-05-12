#lang scribble/manual
@(require scriblib/figure
          redex/pict
          pcf/redex
          cpcf/redex
          spcf/redex
          scpcf/redex
          (for-label pcf/redex
                     cpcf/redex
                     spcf/redex
                     scpcf/redex
                     redex/reduction-semantics))

@(require racket/sandbox
          scribble/eval)
@(define (make-eval lang)
   (call-with-trusted-sandbox-configuration
     (lambda ()
       (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string])
         (let ([the-eval (make-base-eval)])
           (the-eval `(require ,lang))
           the-eval)))))

@(define pcf-eval   (make-eval 'pcf/lang))
@(define cpcf-eval  (make-eval 'cpcf/lang))
@(define spcf-eval  (make-eval 'spcf/lang))
@(define scpcf-eval (make-eval 'scpcf/lang))

@title{PCF with Contracts and Symbolic Values}

@author+email["David Van Horn" "dvanhorn@cs.umd.edu"]

@url{https://github.com/dvanhorn/pcf/}

@table-of-contents[]

@section{Overview}

This package contains a collection of modules for exploring and
experimenting with (variations on) a core typed functional language
based on Plotkin's PCF.

@itemlist[
@item{@bold{PCF}: a core typed language (with natural numbers, errors and
  recursion).}

@item{@bold{Symbolic PCF ('PCF)}: an extension of PCF endowed with a notion
  of "symbolic values", written @racket[(• T)], which represents an
  abstraction of all values of type @racket[T].}

@item{@bold{Contract PCF (CPCF)}: an extension of PCF endowed with
  behavioral software contracts.  Contracts include arbitrary
  predicates written in PCF and higher-order contracts, written
  @racket[(C ... -> C)].  The monitor of a contract against a
  computation is written @racket[(C ⚖ M)].  When a contract fails,
  @racket[blame] is signalled and indicates who is to blame.}

@item{@bold{Symbolic CPCF ('CPCF)}: an extension of Contract PCF
  endowed with symbolic values written @racket[(• T C ...)], which
  represents an abstraction of all values of type @racket[T]
  satisfying contracts @racket[C ...].}]

Each of these languages comes in two forms: as a @literal{#lang} language
and a Redex model.  The syntax of each Redex model is further broken
down in to two components: the source code syntax
(@racket[PCF-source], @racket[CPCF-source], etc.), which is what
programmers write, and the instrumented syntax used during reduction
(@racket[PCF], @racket[CPCF], etc.).

@section{PCF}

@subsection[#:tag "pcf/lang"]{Language}

@defmodulelang[pcf]

PCF is a core typed call-by-value functional programming language.  It
has natural numbers, a few operations, conditionals, recursive
functions and applications.  Conditionals take the form of @code{(if0
e1 e2 e3)}, where @code{0} is interpreterd as ``true.''

@figure["PCF Syntax" "PCF Syntax"]{
@racketgrammar*[#:literals (λ if0 err add1 sub1 * + quotient pos? -> nat :)
                [PCF (code:line M ...)]
                [M X V (M M ...) (μ (X : T) M) (if0 M M M) (err T string)]
                [V natural O (λ ([X : T] ...) M)]
                [O add1 sub1 * + quotient pos?]
                [T nat (T ... -> T)]]}

Some simple examples:

@interaction[#:eval pcf-eval
5
(λ ([x : nat]) x)
((λ ([x : nat]) x) 5)
(if0 1 2 3)
(if0 0 (add1 2) 8)
]

The following interactions demonstrate static type errors and dynamic
run-time errors:

@interaction[#:eval pcf-eval
(add1 add1)
(quotient 5 0)
(err nat "an error")
]

As required by Mass law:
@interaction[#:eval pcf-eval
((μ (fact : (nat -> nat))
    (λ ([n : nat])
      (if0 n
           1
           (* n (fact (sub1 n))))))
 5)
]

@subsection[#:tag "pcf/redex"]{Model}

@defmodule[pcf/redex]

@defidform[#:kind "language" PCF-source]
@defidform[#:kind "language" PCF]
@figure["PCF" (list "The " (racket PCF-source) " and " (racket PCF) " language")
              (render-language PCF-source)
              (render-language PCF)]

@defthing[v reduction-relation?]

@(figure "v" (list "Reduction relation " (racket v)) (render-reduction-relation v #:style 'horizontal))

@defthing[-->v reduction-relation?]

Contextual closure of @racket[v] over evaluation contexts.

@;figure["-->v" (list "Reduction relation " (racket -->v)) (render-reduction-relation -->v #:style 'horizontal)]

@defidform[#:kind "judgment-form" δ]

@figure["δ" (list "Primitive application " (racket δ))]{
@(render-judgment-form δ)

@(render-metafunction δf)}

@defidform[#:kind "judgment form" typeof]

@figure["typeof" (list "Typing judgment " (racket typeof)) (render-judgment-form typeof)]

This typing judgment is defined in terms of the more general
@racket[typeof/contract/symbolic] judgment, which operates on a
superset of @racket[PCF].

@defproc[(typable? (m (redex-match PCF M))) boolean?]{Is @racket[m] a well-typed PCF term?}

@section{CPCF}

@subsection[#:tag "cpcf/lang"]{Language}

@defmodulelang[cpcf]

CPCF extends PCF with a form @code{(C ⚖ M)} for monitoring expressions
with @deftech{contract}s.  A contract @code{C} is either a predicate
or a function contract constructed out of domain and range contracts.

@figure["CPCF Syntax" "CPCF Syntax, extending PCF"]{
@racketgrammar*[#:literals (λ if0 err add1 sub1 * + quotient pos? -> nat : ⚖)
                [M .... (C ⚖ M)]
                [C M (C ... -> C)]]}

Here are some simple examples.  Recall that @code{(λ ([x : nat]) x)}
is the ``is zero?'' predicate:
@interaction[#:eval cpcf-eval
((λ ([x : nat]) x) ⚖ 3)
((λ ([x : nat]) x) ⚖ 0)
(((λ ([x : nat]) x) -> (λ ([x : nat]) x))
 ⚖
 (λ ([x : nat]) x))

((((λ ([x : nat]) x) -> (λ ([x : nat]) x))
  ⚖
  (λ ([x : nat]) x))
 0)

((((λ ([x : nat]) x) -> (λ ([x : nat]) 1))
  ⚖
  (λ ([x : nat]) x))
 0)

((((λ ([x : nat]) 1) -> (λ ([x : nat]) x))
  ⚖
  (λ ([x : nat]) x))
 0)
]

@subsection[#:tag "cpcf/redex"]{Model}

@defmodule[cpcf/redex]

@defidform[#:kind "language" CPCF-source]
@defidform[#:kind "language" CPCF]
@figure["CPCF" (list "The " (racket CPCF-source) " and " (racket CPCF) " languages")]{
@render-language[CPCF-source]

@render-language[CPCF]}

@defthing[cv reduction-relation?]
@figure["cv" (list "Reduction relation " (racket cv)) (render-reduction-relation cv #:style 'horizontal)]

@defthing[-->cv reduction-relation?]

Contextual closure of @racket[cv] over evaluation contexts.

@;figure["-->cv" (list "Reduction relation " (racket -->cv)) (render-reduction-relation -->cv #:style 'horizontal)]

@defidform[#:kind "judgment form" typeof/contract]

@figure["typeof/contract" (list "Typing judgment " (racket typeof/contract))]{
@(render-judgment-form typeof/contract)

@;(render-judgment-form typeof-contract)
}

@defproc[(typable/contract? (m (redex-match CPCF M))) boolean?]{Is @racket[m] a well-typed PCF term?}

@section{SPCF}

@subsection[#:tag "spcf/lang"]{Language}

Symbolic PCF extends PCF with a notion of @deftech{symbolic value}s.  A
symbolic value @code{(• T)} represents the set of values of type
@code{T}.  With the introduction of symbolic values, the reduction
semantics becomes non-deterministic.  The semantics is sound in the
sense that it accounts for the behavior of all values in the set
abstracted by @code{(• T)}.

An important aspect of this semantics is the handling of functions
that escape to unknown contexts, i.e. when a function is an argument
of a symbolic fuction.  The @racket[havoc] metafunction is used in
this case to find any errors that can arise from the use of the
function in an unknown context.

@defmodulelang[spcf]

@figure["SPCF Syntax" "SPCF Syntax, extending PCF"]{
@racketgrammar*[#:literals (λ if0 err add1 sub1 * + quotient pos? -> nat : •)
                [V .... (• T)]]}

@interaction[#:eval spcf-eval
(if0 (• nat) 1 2)

(add1 (if0 (• nat) 1 2))

(add1 (if0 (• nat) 1 (• nat)))

((λ ([x : nat]) 1) 2)

((λ ([f : (nat -> nat)])
   (f (f 5)))
 (λ ([n : nat])
   (quotient 625 n)))

((• ((nat -> nat) -> nat))
 (λ ([n : nat])
   (quotient 625 n)))

((λ ([f : (nat -> nat)])
   (f (f 5)))
 (• (nat -> nat)))
]

@subsection[#:tag "spcf/redex"]{Model}

@defmodule[spcf/redex]

@defidform[#:kind "language" SPCF]
@figure["SPCF" (list "The " (racket SPCF) " language")]{
@render-language[SPCF]}

@defthing[sv reduction-relation?]
@figure["sv" (list "Reduction relation " (racket sv)) (render-reduction-relation sv #:style 'horizontal)]

@figure["havoc" (list "Havoc ") (render-metafunction havoc)]

@defthing[-->sv reduction-relation?]

Contextual closure of @racket[sv] over evaluation contexts.

@;figure["-->cv" (list "Reduction relation " (racket -->cv)) (render-reduction-relation -->cv #:style 'horizontal)]

@defidform[#:kind "judgment-form" δ^]

@figure["δ^" (list "Abstract primitive application " (racket δ^))]{
@render-judgment-form[δ^]}

@defidform[#:kind "judgment form" typeof/symbolic]

@figure["typeof/symbolic" (list "Typing judgment " (racket typeof/symbolic)) (render-judgment-form typeof/symbolic)]

@defproc[(typable/symbolic? (m (redex-match SPCF M))) boolean?]{Is @racket[m] a well-typed SPCF term?}

@section{SCPCF}

@defmodulelang[scpcf]

@subsection[#:tag "scpcf/lang"]{Language}

Symbolic CPCF is an extension of Symbolic PCF which incorporates
contracts as in CPCF and usese contracts in symbolic values to refine
its set of values.  The symbolic value @code{(• T C ...)} represents
the set of all values that are of type @code{T} and satisfy each
contract in @code{C ...}.

@figure["SCPCF Syntax" "SCPCF Syntax, extending CPCF"]{
@racketgrammar*[#:literals (λ if0 err add1 sub1 * + quotient pos? -> nat : ⚖)
                [V .... (• T C ...)]]}

@interaction[#:eval scpcf-eval
(add1 (• nat pos?))

((• ((nat -> nat) -> nat))
 (λ ([n : nat])
   (quotient 625 n)))

((• ((nat -> nat) -> nat))
 ((pos? -> (λ ([x : nat]) 0))
  ⚖
  (λ ([n : nat])
    (quotient 625 n))))
]

@subsection[#:tag "scpcf/redex"]{Model}

@defmodule[scpcf/redex]

@defidform[#:kind "language" SCPCF]
@figure["SCPCF" (list "The " (racket SCPCF) " language")]{
@render-language[SCPCF]}

@;defthing[scv reduction-relation?]
@;figure["scv" (list "Reduction relation " (racket scv)) (render-reduction-relation scv #:style 'horizontal)]

@;figure["havoc" (list "Havoc ") (render-metafunction havoc)]

@;defthing[-->scv reduction-relation?]

Contextual closure of @racket[scv] over evaluation contexts.

@defidform[#:kind "judgment form" typeof/contract/symbolic]

@figure["typeof/contract/symbolic" (list "Typing judgment " (racket typeof/contract/symbolic)) (render-judgment-form typeof/contract/symbolic)]

@defproc[(typable/contract/symbolic? (m (redex-match SCPCF M))) boolean?]{Is @racket[m] a well-typed SCPCF term?}

