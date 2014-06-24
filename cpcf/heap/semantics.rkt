#lang racket
(provide cσ cvσ -->cvσ ∅)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/syntax)

(define cσ
  (reduction-relation
   CPCFΣ #:domain (M Σ)
   (--> ((M L_+ L_- C ⚖ P) Σ)
        ((if0 (@ 'Λ M P) P (blame L_+ C M V)) Σ)
        (where (& A) P)
        (where V (get Σ A))
        ?)
   (--> (((C_1 ..._1 -> C_0) L_+ L_- C ⚖ (& A)) Σ)
        ((λ ([X : T] ...)
           (C_0 L_+ L_- C ⚖
                (@ 'Λ (λ ([X : T] ...) M)
                   (C_1 L_- L_+ C ⚖ X) ...)))
         Σ)         
        (where (λ ([X : T] ..._1) M) (get Σ A))
        η)))

(define cvσ
  (union-reduction-relations cσ (extend-reduction-relation vσ CPCFΣ)))

(define con-abortσ
  (reduction-relation
   CPCFΣ #:domain (M Σ)
   (--> ((in-hole E B) Σ) (B Σ)
        (where #t (not-mt? E))
        con-abort)))

(define -->cvσ 
  (union-reduction-relations (liftσ CPCFΣ cvσ)
                             con-abortσ
                             (extend-reduction-relation err-abortσ CPCFΣ)))
