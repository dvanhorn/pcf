#lang racket
(provide v err-abort -->v δ δf)
(require redex/reduction-semantics
         pcf/syntax
         pcf/private/subst)

(define v
  (reduction-relation
   PCF #:domain M
   (--> ((λ ([X : T] ..._1) M) V ..._1)
	(subst (X V) ... M)
	β)
   (--> (μ (X : T) V)
        (subst (X (μ (X : T) V)) V)
        μ)
   (--> (O V ...) M
	(judgment-holds (δ O (V ...) M))
	δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
	(side-condition (not (zero? (term N))))
	if0-f)))

(define err-abort
  (reduction-relation
   PCF #:domain M
   (--> (in-hole E (err T string))
	(err T string)
	(where #t (not-mt? E))
	
         pcf/private/subst)))

(define -->v
  (union-reduction-relations (context-closure v PCF E) err-abort))

(define-metafunction PCF
  not-mt? : E -> #t or #f
  [(not-mt? hole) #f]
  [(not-mt? E) #t])

(define-judgment-form PCF
  #:mode (δ I I O)
  ;; Using this contract will make v non-reusable.
  ;#:contract (δ O (V ...) M)
  [(δ O (N_0 ...) M)
   (where M (δf O (N_0 ...)))])

(define-metafunction PCF
  δf : O (V ...) -> M
  [(δf add1 (N))           ,(add1 (term N))]
  [(δf sub1 (N))           ,(max 0 (sub1 (term N)))]
  [(δf * (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf pos? (0))            1]
  [(δf pos? (N))            0]
  [(δf quotient (N_0 0))    (err nat "Divide by zero")]
  [(δf quotient (N_0 N_1)) ,(quotient (term N_0) (term N_1))])
