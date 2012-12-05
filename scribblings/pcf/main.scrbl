#lang scribble/doc
@(require scribble/manual
          scriblib/figure
          redex/pict
          pcf/redex
          cpcf/redex
          spcf/redex
          (for-label pcf/redex
                     cpcf/redex
                     spcf/redex
                     redex/reduction-semantics))

@title{PCF with Contracts and Symbolic Values}

@author+email["David Van Horn" "dvanhorn@ccs.neu.edu"]

@table-of-contents[]

@section{PCF}

@defmodule[pcf/redex]

@defidform[#:kind "language" PCF]
@figure["PCF" (list "The " (racket PCF) " language") (render-language PCF)]

@defthing[v reduction-relation?]

@(figure "v" (list "Reduction relation " (racket v)) (render-reduction-relation v #:style 'horizontal))

@defthing[-->v reduction-relation?]

Contextual closure of @racket[v] over evaluation contexts.

@;figure["-->v" (list "Reduction relation " (racket -->v)) (render-reduction-relation -->v #:style 'horizontal)]

@defidform[#:kind "judgment-form" δ]

@figure["δ" (list "Primitive application " (racket δ))]{
@(render-judgment-form δ)

@(render-metafunction δf)}

@defidform[#:kind "metafunction" inj-v]

@render-metafunction[inj-v]

@defidform[#:kind "judgment form" typeof]

@figure["typeof" (list "Typing judgment " (racket typeof)) (render-judgment-form typeof)]

@defproc[(typable? (m (redex-match PCF M))) boolean?]{Is @racket[m] a well-typed PCF term?}

@section{CPCF}

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

@defidform[#:kind "metafunction" inj-cv]

@render-metafunction[inj-cv]

@render-metafunction[lab/context]

@render-metafunction[lab-c/context]

@defidform[#:kind "judgment form" typeof/contract]

@figure["typeof/contract" (list "Typing judgment " (racket typeof/contract))]{
@(render-judgment-form typeof/contract)

@(render-judgment-form typeof-contract)}

@defproc[(typable/contract? (m (redex-match CPCF M))) boolean?]{Is @racket[m] a well-typed PCF term?}

@section{SPCF}

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

@defidform[#:kind "metafunction" inj-sv]

@render-metafunction[inj-sv]

@defidform[#:kind "judgment form" typeof/symbolic]

@figure["typeof/symbolic" (list "Typing judgment " (racket typeof/symbolic)) (render-judgment-form typeof/symbolic)]

@defproc[(typable/symbolic? (m (redex-match SPCF M))) boolean?]{Is @racket[m] a well-typed SPCF term?}

@index-section[]