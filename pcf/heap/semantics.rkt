#lang racket
(provide vσ -->vσ alloc put get ∅)
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

(define -->vσ
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> ((in-hole E M) Σ)
        ((in-hole E M_1) Σ_1)
        (where (_ ... (M_1 Σ_1) _ ...)
               ,(apply-reduction-relation vσ (term (M Σ)))))))

(define-metafunction PCFΣ
  alloc : Σ -> A
  [(alloc Σ)
   ,(add1 (apply max 0 (hash-keys (term Σ))))])

(define-metafunction PCFΣ
  get : Σ A -> V
  [(get Σ A)
   ,(hash-ref (term Σ) (term A))])

(define-metafunction PCFΣ
  put : Σ A V -> Σ
  [(put Σ A V)
   ,(hash-set (term Σ) (term A) (term V))])

(define-term ∅ ,(hash))