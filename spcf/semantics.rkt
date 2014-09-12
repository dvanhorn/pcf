#lang racket
(provide sv -->sv havoc δ^)
(require redex/reduction-semantics
         spcf/syntax
         pcf/private/subst
         pcf/semantics)

(define sv
  (extend-reduction-relation v
   SPCF #:domain M
   (--> Ω Ω Ω)
   (--> (μ (X : T) V) (subst (X (• T)) V) μ) ;; μ^
   (--> (@ L (• (T_0 ..._1 -> T)) V ..._1) (• T) β•)
   (--> (@ L (• (T_0 ..._1 T T_1 ... -> T_o))
	     V_0 ..._1 V V_1 ...)
	(havoc T V)
	havoc)
   (--> (@ L O V ...) M
	(judgment-holds (δ^ O L (V ...) M))
	δ^)
   (--> (if0 (• nat) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat) M_0 M_1) M_1 if•-f)))

(define-metafunction SPCF
  havoc : T M -> M
  [(havoc nat M) (@ Λ (λ ([y : nat]) Ω) M)]
  [(havoc (T_0 ... -> T_1) M)
   (havoc T_1 (@ Λ M (• T_0) ...))])

(define-metafunction SPCF
  not-zero? : any -> #t or #f
  [(not-zero? 0) #f]
  [(not-zero? any) #t])

(define-metafunction SPCF
  not-div? : any -> #t or #f
  [(not-div? ÷) #f]
  [(not-div? any) #t])

(define -->sv
  (union-reduction-relations (context-closure sv SPCF E)
                             (extend-reduction-relation err-abort SPCF)))

(define-judgment-form SPCF
  #:mode (δ^ I I I O)
  #:contract (δ^ O L (V ...) M)
  [(δ^ ÷ L (any (• nat)) (• nat))]
  [(δ^ ÷ L (any (• nat)) (err L nat "Divide by zero"))]
  [(δ^ ÷ L ((• nat) 0)   (err L nat "Divide by zero"))]
  [(δ^ ÷ L ((• nat) N)   (• nat))
   (side-condition (not-zero? N))]
  [(δ^ O L (any_0 ... (• nat) any_1 ...) (• nat))
   (side-condition (not-div? O))])
