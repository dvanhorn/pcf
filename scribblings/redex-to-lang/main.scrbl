#lang scribble/manual
@(require scribble/manual
          scriblib/figure
          redex/pict
          pcf/redex
          scpcf/redex
          (for-label pcf/redex
                     scpcf/redex
                     redex/reduction-semantics))

@(require racket/sandbox
          scribble/eval)
@(define (make-eval . reqs)
   (call-with-trusted-sandbox-configuration
     (lambda ()
       (parameterize ([sandbox-output 'string]
                      [sandbox-error-output 'string])
         (let ([the-eval (make-base-eval)])
           (the-eval `(require ,@reqs))
           the-eval)))))

@(define pcf-eval   (make-eval 'pcf/lang))
@(define redex-eval (make-eval 'redex/reduction-semantics
                               'redex/pict
                               'pcf/redex))



@title{From Redex Models to Programming Languages}

@author+email["David Van Horn" "dvanhorn@ccs.neu.edu"]

@section{Overview}

Racket is not merely a programming language--it is a @emph{programming
language for making programming languages}.  There are several
features that make Racket unique in its ability to support the
creation of new languages, but in this article we're going to look at
how to combine two of them---Redex, a domain-specific language for
writing mathematical models of programming languages, and the
@tt{#lang} mechanism for creating new programming languages that
Racket modules can be written in.  In the end, this provides a method
for developing programming language @emph{models} in to fully featured
@tt{#lang} @emph{languages} integrated into the Racket
ecosystem.


@section{A Redex Model of PCF}

Let's start by building a model of a very simple, typed functional
programming language based on Plotkin's PCF language @cite{PCF}.  Although simple,
PCF contains all the essential elements of a real programming
language.  Scaling the approach of these notes up to a more
sophisticated language is just a small matter of semantic hacking.

@subsection[#:tag "pcf/lang"]{Syntax}

@;defmodulelang[pcf]

PCF is a core typed functional programming language.  For values, it
includes natural numbers, functions, and primitive operations.
Expressions include conditions, applications, recursive binding, and
errors.  The syntax of PCF is given in @figure-ref{redex-to-lang/PCF}.

@figure["redex-to-lang/PCF-source" (list "The " (racket PCF-source) " language") 
                                   (parameterize ([render-language-nts '(T M V S N O X)])
                                     (render-language PCF-source))]

@margin-note{For more on using Redex, see the book @emph{Semantics Engineering
with PLT Redex} @cite{Redex}.}                                                         
            
To model this language in Redex, we first need to define
the grammar of the language:
@codeblock|{
#lang racket
(require redex/reduction-semantics)
(define-language PCF-source
  ;; Types
  (T ::= nat (T ... -> T))
  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) S) (if0 M M M) (err T string))
  ;; Values
  (V ::= N O (λ ([X : T] ...) M))
  ;; Simple terms
  (S ::= V X)
  ;; Naturals
  (N ::= natural)
  ;; Primitive operations
  (O ::= add1 sub1 * + quotient pos?)
  ;; Variables
  (X ::= variable-not-otherwise-mentioned))}|
  
It is no accident that the above definition and that of @figure-ref{redex-to-lang/PCF-source}
look similiar.  In fact, typeset grammar of
@figure-ref{redex-to-lang/PCF-source} is computed from the definition of
@racket[PCF-source].  You can try it for yourself after requiring
the @racket[redex/pict] module:
@(redex-eval '(render-language-nts '(T M V S N O X)))
@interaction[#:eval redex-eval
             (require redex/pict)
             (render-language PCF-source)]


At this point, it's possible to construct PCF programs using Redex's @racket[term]
constructor.  For example, @racket[(term (add1 7))] represents the program
@render-term[PCF-source (add1 7)].  Terms just s-expression values in Racket:
@interaction[#:eval redex-eval
(term (add1 7))
]

In order to @emph{compute} with terms, we need to endow our model with
metafunctions, judgments, and relations.  So let's start with a judgment
about the syntactic correctness of programs, i.e. let's start by
defining what it means for a term to be well-typed.

@figure["redex-to-lang/typeof" (list "Typing judgment " (racket typeof) " (selected cases)")                                
(parameterize [(judgment-form-cases
                (list 0 1 2 3 8 9 10 11 12))]
  (render-judgment-form typeof/contract/symbolic))]

This judgment relies on a @emph{type environment}, @math{Γ}, which not
included in the @racket[PCF-source] definition.  To define type environments,
we add a definition to @racket[PCF-source], shown in @figure-ref{redex-to-lang/PCF-static}.

@figure["redex-to-lang/PCF-static" (list "Type environments")                                
(parameterize ([render-language-nts '(Γ)])
  (render-language PCF-source))]

The code for the typing judgment takes the following form:

@codeblock[#:keep-lang-line? #f]|{
#lang racket
(define-judgment-form PCF-static
  #:mode (typeof I I O)
  #:contract (typeof Γ M T)
  [(typeof Γ (err T string) T)]
  [(typeof Γ N nat)]
  ...)
}|

The rest is straightforward to transliterate from @figure-ref{redex-to-lang/typeof}.
As you'll notice,
there are two cases that rely on a metafunction @racket[extend] to extend
the type environment.  You should be able to write this and the remaining cases
of @racket[typeof] as an exercise.

The @racket[#:mode] and @racket[#:contract] specifications says this judgment takes the environment and
term as inputs, and produces a type as an output.  The judgment is really a @emph{relation}
that has been inductively defined.  Since in general a relation may relate several
ouputs to a single input, the judgment can also be seen as a @emph{function} from
inputs to @emph{sets of} outputs.  To check whether the judgment holds
for specific inputs and outputs, we can query the relation to ask if the
inputs and outputs are related:
@interaction[#:eval redex-eval
                    (judgment-holds (typeof () 7 nat))
                    (judgment-holds (typeof ((f (nat -> nat))) (f 7) nat))
                    (judgment-holds (typeof () (7 add1) nat))]

It's also possible to write patterns for the outputs and @emph{compute}
a set of outputs rather than @emph{check} if a particular one is in the relation:
@interaction[#:eval redex-eval
                    (judgment-holds (typeof () 7 T) T)
                    (judgment-holds (typeof ((f (nat -> nat))) (f 7) T) T)
                    (judgment-holds (typeof () (7 add1) T) T)]

(If you think about it, you can probably convince yourself that @racket[typeof]
relates programs to at most one type.  What does that mean about the
@racket[typeof] relation?  Try to come up with some judgment that relates
inputs to multiple outputs and experiment with it.)

Now that we have the syntax and type judgment defined, let's move on to the
dynamic semantics of @racket[PCF].

@subsection{Semantics}

The semantics of PCF is given by a @emph{reduction relation} which is
relation on program text that transforms a fragment of text into
a simpler expression.  You should think of this relation as specifying a kind
of algebra of PCF programs: you can simplyify programs in the same way you
can simplify arithmetic expressions, but instead of appealing to axioms 
such as @tt{(@math{n} + @math{m})} @math{→ n+m}, you have axiom for function
application, branching with conditional expressions, and recursion, etc.  These
axioms are shown in @figure-ref{redex-to-lang/v}.

@(figure "redex-to-lang/v" (list "Reduction relation " (racket v-source)) (render-reduction-relation v-source #:style 'horizontal))

Finally, we can now compute with programs by using the reduction relation,
which can be applied by using Redex's @racket[apply-reduction-relation]:
@interaction[#:eval redex-eval
             (apply-reduction-relation v-source 
               (term (add1 7)))
             (apply-reduction-relation v-source 
               (term ((λ ([f : (nat -> nat)]) (f 3)) sub1)))]

Notice that in the second example, the relation produced @racket[(sub1 3)],
not @racket[2].  That's because @racket[apply-reduction-relation] produces
the set of terms related to the input by exactly one use of @racket[v-source].
If you'd like to repeatedly apply @racket[v-source] until the program
is in simplest terms, use @racket[apply-reduction-relation*]:
@interaction[#:eval redex-eval
             (apply-reduction-relation* v-source 
               (term ((λ ([f : (nat -> nat)]) (f 3)) sub1)))]

While the @racket[v-source] relates terms that can immediately be reduced,
it doesn't say anything about terms that @emph{contain} reducible terms, but
themselves are not reducible.  For example, @racket[(term (sub1 (add1 5)))]
does not match the left-hand-side of any axiom in @racket[v-source].  To enable
such reduction, we need to specify a grammar of @emph{evaluation contexts}
that specify @emph{where} a reduction may be applied.  Here is a definition
of evaluation contexts that gives a left-to-right order of evaluation:

@centered{
@(parameterize [(render-language-nts '(E))]
   (render-language PCF-source))}

Once we add the definition of evaluation contexts to @racket[PCF-source],
it is possible to define the @emph{contextual closure} of @racket[v]
with respect to @render-term[PCF-source E], which we call @racket[-->v-source]:

@interaction[#:eval redex-eval
(define -->v-source
  (context-closure v-source PCF-source E))
(apply-reduction-relation* -->v-source 
  (term (sub1 (add1 5))))
(apply-reduction-relation* -->v-source
  (term ((μ (fact : (nat -> nat))
            (λ ([n : nat])
              (if0 n
                   1
                   (* n (fact (sub1 n))))))
         5)))
]

At this point, we have the basics of a working model for PCF.
We might go further and turn some of our examples into test cases, perhaps
state some theorems of PCF and use random testing to try to disprove them,
or write a document that uses @racket[render-language] and friends to typeset
the various definitions from our PCF model (a document such as this!).

But the thing we probably won't be doing a lot of is @emph{programming in PCF}.
It's just too cumbersome.  To write a PCF program you have to load the Redex
library and all of the PCF definitions, then you have to use @racket[term]
to construct programs and then use @racket[apply-reduction-relation*] to compute
with them.  In other words, we have to embed a PCF program as a datum within
a Redex program that interprets it.  But Racket makes it easy to go from
an interpreter to a @tt{#lang} language.  In the next sections, we'll explore
how to build a @tt{#lang} for PCF so that it's just as natural to write PCF
programs as it is Racket programs.

@section{Building languages}

Let's put the horse before the cart and see the end result of @emph{what} we'll
accomplish in this section, before seeing @emph{how}.  Our goal is a @tt{#lang pcf}
language that enables us to write PCF programs like this:

@centered{@image[#:suffixes '(".pdf" ".png") #:scale .8]{pcf}}

@;defthing[-->v reduction-relation?]

Contextual closure of @racket[v] over evaluation contexts.

@;figure["-->v" (list "Reduction relation " (racket -->v)) (render-reduction-relation -->v #:style 'horizontal)]

@;defidform[#:kind "judgment-form" δ]

@figure["redex-to-lang/δ" (list "Primitive application " (racket δ))]{
@(render-judgment-form δ)

@(render-metafunction δf)}

@;defidform[#:kind "judgment form" typeof]


@;defproc[(typable? (m (redex-match PCF M))) boolean?]{Is @racket[m] a well-typed PCF term?}

@figure["redex-to-lang/PCF Syntax" "PCF Syntax"]{
@racketgrammar*[#:literals (λ if0 err add1 sub1 * + quotient pos? -> nat :)
                [PCF (code:line M ...)]
                [M X V (M M ...) (μ (X : T) M) (if0 M M M) (err T string)]
                [V natural O (λ ([X : T] ...) M)]
                [O add1 sub1 * + quotient pos?]
                [T nat (T ... -> T)]]}

@interaction[#:eval pcf-eval
5
(λ ([x : nat]) x)
((λ ([x : nat]) x) 5)
(if0 1 2 3)
(if0 0 (add1 2) 8)

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


@bibliography[
 @bib-entry[#:key "PCF"
            #:title "LCF considered as a programming language"
            #:author "Gordon Plotkin"
            #:date "1977"
            #:location "Theoretical Computer Science 5: 223–255"
            #:url "http://homepages.inf.ed.ac.uk/gdp/publications/LCF.pdf"]
 @bib-entry[#:key "Redex"
            #:title "Semantics Engineering with PLT Redex"
            #:author "Matthias Felleisen, Robert Bruce Findler and Matthew Flatt"
            #:date "July, 2009"
            #:location "MIT Press"]]
