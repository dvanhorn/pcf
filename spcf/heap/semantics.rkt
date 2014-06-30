#lang racket
(provide svσ -->svσ ∅ sfoldσ)
(require redex/reduction-semantics 
         pcf/heap/semantics
         spcf/semantics
         pcf/private/subst
         spcf/heap/syntax)

(define svσ
  (extend-reduction-relation vσ
   SPCFΣ #:domain (M Σ)
   (--> (Ω Σ) (Ω Σ) Ω)        
   (--> ((μ (X : T) V) Σ)
        ((subst (X (& A)) V) (put Σ A (• T)))
        (where A (alloc Σ))
        μ) ;; μ^
   (--> ((@ L (& A_f) P ..._1) Σ) 
        ((• T) Σ) 
        (where (• (T_0 ..._1 -> T)) (get Σ A_f))
        β•)
   (--> ((@ L (& A_f) P_0 ..._1 P P_1 ...)
         Σ)
        ((havoc T P) Σ)
        (where (• (T_0 ..._1 T T_1 ... -> T_o))
               (get Σ A_f))
        havoc)
   (--> ((@ L (& A_O) (& A_V) ...) Σ)
        (M Σ)
        (where O (get Σ A_O))
        (where (V ...) ((get Σ A_V) ...))
        (judgment-holds (δ^ O L (V ...) M))
        δ^)
   (--> ((if0 (& A) M_0 M_1) Σ)
        (M_0 Σ)
        (where (• nat) (get Σ A))
        if•-t)
   (--> ((if0 (& A) M_0 M_1) Σ)
        (M_1 Σ)
        (where (• nat) (get Σ A))
        if•-f)))

(define -->svσ 
  (union-reduction-relations (liftσ SPCFΣ svσ) 
                             (extend-reduction-relation err-abortσ SPCFΣ)))

(define-metafunction/extension foldσ SPCFΣ
  sfoldσ : (M Σ) -> M
  [(sfoldσ ((• T) Σ)) (• T)])
