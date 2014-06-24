#lang racket
(provide vσ err-abortσ -->vσ alloc put get ∅ liftσ not-mt?)
(require pcf/heap/syntax
         pcf/semantics
         pcf/private/subst
         redex/reduction-semantics)

(define vσ
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> (V Σ) ((& A) (put Σ A V))
        (where A (alloc Σ)))
   (--> ((@ L (& A_f) P_V ..._1) Σ)
        ((subst (X P_V) ... M) Σ)
        (where (λ ([X : T] ..._1) M)
               (get Σ A_f))
        β)
   (--> ((μ (X : T) V) Σ)
        ((& A) (put Σ A (subst (X (& A)) V)))
        (where A (alloc Σ))
        μ)
   (--> ((@ L (& A_O) (& A_V) ...) Σ)
        (M Σ)
        (where O (get Σ A_O))
        (where (V ...) ((get Σ A_V) ...))
        (judgment-holds (δ O L (V ...) M))
        δ)
   (--> ((if0 (& A) M_0 M_1) Σ)
        (M_0 Σ)
        (where 0 (get Σ A))
        if0-t)
   (--> ((if0 (& A) M_0 M_1) Σ)
        (M_1 Σ)
        (where N (get Σ A))
        (judgment-holds (nonzero? N))
        if0-f)))


(define-syntax-rule (liftσ L r)
  (reduction-relation
   L #:domain (M Σ)
   (--> ((in-hole E M) Σ)
        ((in-hole E M_1) Σ_1)
        (where (_ (... ...) (M_1 Σ_1) _ (... ...))
               ,(apply-reduction-relation r (term (M Σ)))))))

(define err-abortσ
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> ((in-hole E (err L T string)) Σ)
        ((err L T string) Σ)
        (where #t (not-mt? E))
	err-abort)))

(define -->vσ 
  (union-reduction-relations (liftσ PCFΣ vσ) err-abortσ))


(define-metafunction PCFΣ
  alloc : any_Σ -> any_A
  [(alloc any_Σ)
   ,(add1 (apply max 0 (hash-keys (term any_Σ))))])

(define-metafunction PCFΣ
  get : any_Σ any_A -> any_V
  [(get any_Σ any_A)
   ,(hash-ref (term any_Σ) (term any_A))])

(define-metafunction PCFΣ
  put : any_Σ any_A any_V -> any_Σ
  [(put any_Σ any_A any_V)
   ,(hash-set (term any_Σ) (term any_A) (term any_V))])

(define-term ∅ ,(hash))