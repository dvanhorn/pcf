#lang racket
(provide cσ cvσ con-abortσ -->cvσ ∅ cfoldσ injσ)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/syntax)

(define cσ
  (reduction-relation
   CPCFΣ #:domain (M Σ)
   (--> ((P_F L_+ L_- C ⚖ P) Σ)
        ((if0 (@ 'Λ P_F P) P (blame L_+ C P_F P)) Σ)
        (where (& A_C) P_F)
        (where (F _ ...) (get Σ A_C))
        ?)
   (--> (((& A_C) L_+ L_- C_n ⚖ (& A_V)) Σ)
	((λ ([X : T] ...)
           (P_n L_+ L_- C_n ⚖
                (@ Λ (& A_V)
                   (P_1 L_- L_+ C_n ⚖ X) ...)))
         Σ)
        (where ((P_1 ... -> P_n) _ ...) (get Σ A_C))
        (where ((λ ([X : T] ...) M) _ ...) (get Σ A_V))
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


(define-metafunction/extension foldσ CPCFΣ
  cfoldσ : (M Σ) -> M
  [(cfoldσ ((C_0 ... -> C) Σ))
   ((cfoldσ (C_0 Σ)) ... -> (cfoldσ (C Σ)))]
  [(cfoldσ ((C_0 L_0 L_1 C_1 ⚖ M) Σ))
   ((cfoldσ (C_0 Σ)) L_0 L_1 (cfoldσ (C_1 Σ)) ⚖ (cfoldσ (M Σ)))]
  [(cfoldσ ((blame L C_0 C_1 M) Σ))
   (blame L (cfoldσ (C_0 Σ)) (cfoldσ (C_1 Σ)) (cfoldσ (M Σ)))])
