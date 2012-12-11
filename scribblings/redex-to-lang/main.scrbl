#lang scribble/manual
@(require scribble/manual
	  scriblib/figure
	  redex/pict
	  pcf/source
	  (for-label pcf/source
		     racket/sandbox
		     scribble/eval
		     racket/base
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

@(define pcf-eval (make-eval 'pcf/source/lang))
@(define redex-eval
   (make-eval 'redex/reduction-semantics
	      'redex/pict
	      'pcf/source))



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
@tt{#lang} @emph{languages} integrated into the Racket ecosystem.


@section{A Redex Model of PCF}

Let's start by building a model of a very simple, typed functional
programming language based on Plotkin's PCF language @cite{PCF}.
Although simple, PCF contains all the essential elements of a real
programming language.  Scaling the approach of these notes up to a
more sophisticated language is just a small matter of semantic
hacking.

@subsection[#:tag "pcf/lang"]{Syntax}

@;defmodulelang[pcf]

PCF is a core typed functional programming language.  For values, it
includes natural numbers, functions, and primitive operations.
Expressions include conditions, applications, recursive binding, and
errors.  The syntax of PCF is given in @figure-ref{redex-to-lang/PCF}.

@figure["redex-to-lang/PCF-source" (list "The " (racket PCF-source) " language")
				   (parameterize ([render-language-nts '(T M V S N O X)])
				     (render-language PCF-source))]

@margin-note{For more on using Redex, see the book @emph{Semantics
Engineering with PLT Redex} @cite{Redex}.}

To model this language in Redex, we first need to define the grammar
of the language:

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
  (O ::= add1 sub1 * + quotient)
  ;; Variables
  (X ::= variable-not-otherwise-mentioned))}|

It is no accident that the above definition and that of
@figure-ref{redex-to-lang/PCF-source} look similiar.  In fact, typeset
grammar of @figure-ref{redex-to-lang/PCF-source} is computed from the
definition of @racket[PCF-source].  You can try it for yourself after
requiring the @racket[redex/pict] module:

@(redex-eval '(render-language-nts '(T M V S N O X)))
@interaction[#:eval redex-eval
	     (require redex/pict)
	     (render-language PCF-source)]


At this point, it's possible to construct PCF programs using Redex's
@racket[term] constructor.  For example, @racket[(term (add1 7))]
represents the program @render-term[PCF-source (add1 7)].  Terms just
s-expression values in Racket:

@interaction[#:eval redex-eval
(term (add1 7))
]

In order to @emph{compute} with terms, we need to endow our model with
metafunctions, judgments, and relations.  So let's start with a
judgment about the syntactic correctness of programs, i.e. let's start
by defining what it means for a term to be well-typed.

@figure["redex-to-lang/typeof"
	(list "Typing judgment " (racket typeof) " (selected cases)")
	(parameterize ([judgment-form-cases
			'(0 1 2 3 9 11)])
	  (render-judgment-form typeof))]

This judgment relies on a @emph{type environment}, @math{Γ}, which not
included in the @racket[PCF-source] definition.  To define type
environments, we add a definition to @racket[PCF-source], shown in
@figure-ref{redex-to-lang/PCF-static}.

@figure["redex-to-lang/PCF-static" (list "Type environments")
(parameterize ([render-language-nts '(Γ)])
  (render-language PCF-source))]

The code for the typing judgment takes the following form:

@codeblock[#:keep-lang-line? #f]|{
#lang racket
(define-judgment-form PCF-source
  #:mode (typeof I I O)
  #:contract (typeof Γ M T)
  [(typeof Γ (err T string) T)]
  [(typeof Γ N nat)]
  ...)
}|

The rest is straightforward to transliterate from
@figure-ref{redex-to-lang/typeof}.  As you'll notice, there are two
cases that rely on a metafunction @racket[extend] to extend the type
environment.  You should be able to write this and the remaining cases
of @racket[typeof] as an exercise.

The @racket[#:mode] and @racket[#:contract] specifications says this
judgment takes the environment and term as inputs, and produces a type
as an output.  The judgment is really a @emph{relation} that has been
inductively defined.  Since in general a relation may relate several
ouputs to a single input, the judgment can also be seen as a
@emph{function} from inputs to @emph{sets of} outputs.  To check
whether the judgment holds for specific inputs and outputs, we can
query the relation to ask if the inputs and outputs are related:

@interaction[#:eval redex-eval
		    (judgment-holds (typeof () 7 nat))
		    (judgment-holds (typeof ((f (nat -> nat))) (f 7) nat))
		    (judgment-holds (typeof () (7 add1) nat))]

It's also possible to write patterns for the outputs and
@emph{compute} a set of outputs rather than @emph{check} if a
particular one is in the relation:

@interaction[#:eval redex-eval
		    (judgment-holds (typeof () 7 T) T)
		    (judgment-holds (typeof ((f (nat -> nat))) (f 7) T) T)
		    (judgment-holds (typeof () (7 add1) T) T)]

(If you think about it, you can probably convince yourself that
@racket[typeof] relates programs to at most one type.  What does that
mean about the @racket[typeof] relation?  Try to come up with some
judgment that relates inputs to multiple outputs and experiment with
it.)

Now that we have the syntax and type judgment defined, let's move on
to the dynamic semantics of @racket[PCF].

@subsection{Semantics}

The semantics of PCF is given by a @emph{reduction relation} which is
relation on program text that transforms a fragment of text into a
simpler expression.  You should think of this relation as specifying a
kind of algebra of PCF programs: you can simplyify programs in the
same way you can simplify arithmetic expressions, but instead of
appealing to axioms such as @tt{(@math{n} + @math{m})} @math{→ n+m},
you have axiom for function application, branching with conditional
expressions, and recursion, etc.  These axioms are shown in
@figure-ref{redex-to-lang/v}.

@(figure "redex-to-lang/v"
         (list "Reduction relation " (racket v-source))
         (render-reduction-relation v-source #:style 'horizontal))

The @racket[v-source] relation makes use of three helper
metafunctions.  The first, @racket[subst], does substitution.  Rather
than define it yourself, you can import this one from
@racket[pcf/private/subst]---or you can look at how to it yourself in
the @emph{Redex} book.  It does what you might expect:

@interaction[#:eval redex-eval
(require pcf/private/subst)
(term (subst (x 4) (y 7) (+ x (* y x))))]

The second metafunction, @racket[δ], interprets the application of
primitive operations to values.  Its definition is fairly
straightforward, so we leave it to work out the details.  Some
examples:

@interaction[#:eval redex-eval
(term (δf add1 (7)))
(term (δf * (3 4)))
(term (δf quotient (14 3)))
(term (δf quotient (14 0)))]

The third and final metafunction used by @racket[v-source] is
@racket[nonzero?], which by now you should have no problem defining.

Finally, we can now compute with programs by using the reduction
relation, which can be applied by using Redex's
@racket[apply-reduction-relation]:

@interaction[#:eval redex-eval
	     (apply-reduction-relation v-source
	       (term (add1 7)))
	     (apply-reduction-relation v-source
	       (term ((λ ([f : (nat -> nat)]) (f 3)) sub1)))]

Notice that in the second example, the relation produced @racket[(sub1
3)], not @racket[2].  That's because @racket[apply-reduction-relation]
produces the set of terms related to the input by exactly one use of
@racket[v-source].  If you'd like to repeatedly apply
@racket[v-source] until the program is in simplest terms, use
@racket[apply-reduction-relation*]:

@interaction[#:eval redex-eval
	     (apply-reduction-relation* v-source
	       (term ((λ ([f : (nat -> nat)]) (f 3)) sub1)))]

While the @racket[v-source] relates terms that can immediately be
reduced, it doesn't say anything about terms that @emph{contain}
reducible terms, but themselves are not reducible.  For example,
@racket[(term (sub1 (add1 5)))] does not match the left-hand-side of
any axiom in @racket[v-source].  To enable such reduction, we need to
specify a grammar of @emph{evaluation contexts} that specify
@emph{where} a reduction may be applied.  Here is a definition of
evaluation contexts that gives a left-to-right order of evaluation:

@centered{
@(parameterize [(render-language-nts '(E))]
   (render-language PCF-source))}

Once we add the definition of evaluation contexts to
@racket[PCF-source], it is possible to define the @emph{contextual
closure} of @racket[v] with respect to @render-term[PCF-source E],
which we call @racket[-->v-source]:

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

There is one short-coming of the @racket[-->v-source] relation that
you may have noticed.  Programs that produce errors get @emph{stuck},
e.g.:

@interaction[#:eval redex-eval
(apply-reduction-relation* -->v-source
  (term (sub1 (quotient 5 0))))]

In this small example, it's easy to see the program signalled an
error, but in anything much larger it becomes very difficult to spot
what's gone wrong when an error is buried deep inside some evaluation
context.  We can define a reduction relation that handles such
situations by discarding the surrounding context and producing the
error as the overall result of the computation:

@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation err-abort)))

or as code:

@interaction[#:eval redex-eval
(define err-abort
  (reduction-relation
   PCF-source #:domain M
   (--> (in-hole E (err T string)) (err T string)
   (where #t (not-mt? E))
   err-abort)))

(apply-reduction-relation err-abort
  (term (sub1 (err nat "Divide by zero"))))]

This relation relies on a helper metafunction @racket[not-mt?]  that
determines if an evaluation context is not the empty context.  (What
would happen if you left that side condition off?)  Notice how the
rule is context-sensitive: it matches the entire program against an
evaluation context pattern variable and an error term, then throws the
context away on the right-hand-side.  To put incorporate this rule
into the PCF semantics, let's re-define @racket[-->v-source] as the
following, which combines the context closure of @racket[v-source] and
the new @racket[err-abort] relation:

@interaction[#:eval redex-eval
(define -->v-source
  (union-reduction-relations
   (context-closure v-source PCF-source E)
   err-abort))]

Now when an error occurs, it is produced as the result of the
computation:

@interaction[#:eval redex-eval
(apply-reduction-relation* -->v-source
  (term (sub1 (quotient 5 0))))]

So now we have a working interpreter for PCF in the form of the
reflexive, transitive closure of @racket[-->v-source], which is
exactly what @racket[apply-reduction-relation*] computes.  If we
wanted more detail on these computations, we can use some more of the
tools that come with Redex.  For example, by requiring
@racket[redex/gui], we can use the @racket[traces] function for
visualizing the @emph{trace} of computation.  For example, the result
of

@racketblock[
(traces -->v-source
	(term ((λ ([f : (nat -> nat)]) (f (f 2)))
	       (λ ([x : nat]) (* x x)))))]

is an interactive window that shows the intermediate terms of the
computation as a graph labelled with edges corresponding to reduction
axioms, shown in @figure-ref{traces}.

@figure["traces" "Reduction graph"]{
@centered{@image[#:suffixes '(".png" ".pdf") #:scale .8]{traces}}}

Redex also gives you the ability to step through a computation with an
interactive algebraic stepper.  Simply use the @racket[stepper]
function:

@racketblock[
(stepper -->v-source
	 (term ((λ ([f : (nat -> nat)]) (f (f 2)))
		(λ ([x : nat]) (* x x)))))]

which will launch the stepper window shown in @figure-ref{stepper}.

@figure["stepper" "Algebraic stepper"]{
@image[#:suffixes '(".pdf" ".png") #:scale .8]{stepper}}

At this point, we have the basics of a working model for PCF.  We
might go further and turn some of our examples into test cases,
perhaps state some theorems of PCF and use random testing to try to
disprove them, or write a document that uses @racket[render-language]
and friends to typeset the various definitions from our PCF model (a
document such as this!).

But the thing we probably won't be doing a lot of is @emph{programming
in PCF}.  It's just too cumbersome.  To write a PCF program you have
to load the Redex library and all of the PCF definitions, then you
have to use @racket[term] to construct programs and then use
@racket[apply-reduction-relation*] to compute with them.  In other
words, we have to embed a PCF program as a datum within a Redex
program that interprets it.  But Racket makes it easy to go from an
interpreter to a @tt{#lang} language.  In the next sections, we'll
explore how to build a @tt{#lang} for PCF so that it's just as natural
to write PCF programs as it is Racket programs.

@section{Building languages}

Let's put the horse before the cart and see the end result of
@emph{what} we'll accomplish in this section, before seeing
@emph{how}.  Our goal is a @tt{#lang pcf} language that enables us to
write PCF programs like this:

@centered{@image[#:suffixes '(".pdf" ".png") #:scale .8]{pcf}}

In the definitions panel of this DrRacket window, you'll notice we've
indicated PCF as the language this module is written in.  The
subsequent text consists of a PCF expression computing @math{5!}.
After pressing ``Run'', the interactions panel appears and prints the
results from the definitions panel, i.e. @racket[120].  After which,
we can type PCF expressions at the prompt and get results back
interactively.

Not only is it now easier to develop programs written in PCF, it is
also easier to @emph{discuss} PCF programs since we can use the PCF
language directly in documenting examples:

@interaction[#:eval pcf-eval
((μ (fact : (nat -> nat))
    (λ ([n : nat])
      (if0 n
	   1
	   (* n (fact (sub1 n))))))
 5)]

It is worth point out that this is a @emph{different} prompt than the
one used before.  It is a PCF prompt, not a Racket prompt.

Making a @tt{#lang} language for an s-expression-based language
requires two things:

@itemlist[

@item{a @racket[#%module-begin] macro that is given programs in the
definitions panel as arguments, and}

@item{a @racket[#%top-interaction] form that is given an expression
sent to the interactions panel as an argument.}]

(If our language's syntax weren't based on s-expressions, we'd also
have to write a parser.)

We can experiment by defining two macros, @racket[pcf-module] and
@racket[pcf-top] (we've renamed them to the ``@tt{#%}'' special names
in the @racket[provide] clause).  These macros perform the same basic
task, given an expression---or a sequence of expressions in the case
of @racket[pcf-module]--- the macro transforms the PCF expression into
a Racket expression that evaluates it.  So we arrive at the following:

@codeblock[#:keep-lang-line? #t]|{
#lang racket
(provide (rename-out [pcf-top #%top-interaction]
		     [pcf-module #%module-begin]))
(require (for-syntax syntax/parse))
(require redex/reduction-semantics
	 pcf/source)
(define-syntax (pcf-top stx)
  (syntax-parse stx
    [(_ e)
     #'(#%top-interaction .
	 (apply values
		(apply-reduction-relation* -->v-source (term e))))]))

(define-syntax (pcf-module stx)
  (syntax-parse stx
    [(_ e ...)
     #'(#%module-begin
	 (apply values
		(append (apply-reduction-relation* -->v-source (term e))
			...)))]))
}|

Save the above module in @filepath{pcf/source/lang.rkt}.  At this
point, we can write modules in PCF using @tt{#lang s-exp
pcf/source/lang}, but you can make @tt{#lang pcf/source} available by
taking the extra step of creating a file
@filepath{pcf/source/lang/reader.rkt} with the following contents:

@codeblock[#:keep-lang-line? #t]|{
#lang s-exp syntax/module-reader
pcf/source/lang
}|

This is a bit of magic that resides in a specific location that Racket
will use to resolve @tt{#lang}s that start with
@racketmodname[pcf/source].  After this, we have a bonafide language:

@codeblock[#:keep-lang-line? #t]|{
#lang pcf/source
((μ (fib : (nat -> nat))
    (λ ([n : nat])
      (if0 n
	   0
	   (if0 (sub1 n)
		1
		(+ (fib (sub1 n))
		   (fib (sub1 (sub1 n))))))))
 5)
}|

@section{Catching type errors with Check Syntax}

One thing you may notice is that Check Syntax does do a whole lot for
you, which is really too bad considering Check Syntax now runs
continuously in DrRacket.  The problem is that the only thing
@tt{pcf/source} does when it is expanded is convert the text from the
definitions window into a datum.  You can basically write whatever
junk you'd like: so long as the parens balance, DrRacket will be
perfectly happy with it... until you run it.  That's of course not
what DrRacket does for @emph{Racket} programs.  For @tt{#lang racket}
programs, Check Syntax will let you know if you use undefined names,
or leave off an else branch in an if.  In other words, it's
continuously checking the well-formedness of your program.  Wouldn't
it be nice to get some of that chocolate on our peanut-butter?

Well, we have a couple perfectly sensible well-formedness condition on
PCF programs.  One simple one is just to check that the input follows
the grammar of PCF.  We can do one better though and further make sure
the input is well-typed by using the @racket[typeof] judgment from the
PCF model.

@section{Lexical structure}

@section{Highlighting run-time errors}

@section{Typesetting examples}

As mentioned in @secref{Building_languages}, once we have a @tt{#lang}
language, it becomes possible to use that language just like any other
@tt{#lang} language.  For example, in this document, there is a Racket
evaluator for showing examples of using Redex, and there is a PCF
evaluator for showing examples using PCF.  Both are constructed at the
beginning of the Scribble document like so:

@codeblock|{
#lang scribble/manual
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

@(define pcf-eval (make-eval 'pcf/lang))
@(define redex-eval
   (make-eval 'redex/reduction-semantics
	      'redex/pict
	      'pcf/source))}|

at which point it's easy to write code to typeset examples of either
language.  For example this:

@codeblock[#:keep-lang-line? #f]|{
#lang scribble/manual
@(examples #:eval pcf-eval
   ((μ (fact : (nat -> nat))
       (λ ([n : nat])
	 (if0 n
	      1
	      (* n (fact (sub1 n))))))
    5))
}|

will create an example that looks like this:

@(examples #:eval pcf-eval
   ((μ (fact : (nat -> nat))
       (λ ([n : nat])
	 (if0 n
	      1
	      (* n (fact (sub1 n))))))
    5))



@section{Where to Go from Here}


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
