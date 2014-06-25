#lang racket
(provide scvσ -->scvσ ∅)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/semantics
         pcf/private/subst
         (except-in scpcf/semantics δ^)
         scpcf/heap/syntax)

(define scvσ
  (extend-reduction-relation vσ ;; maybe cvσ?
   SCPCFΣ #:domain (M Σ)
   (--> (Ω Σ) (Ω Σ) Ω) ;; Loop

   (--> ((μ (X : T) V) Σ)
        ((subst (X (& A)) V) (put Σ A (• T)))
        (where A (alloc Σ))
        μ) ;; μ^
     
   (--> ((@ L (& A_f) P ...) Σ)
        ((• T C ...) Σ)        
        (where (• (T_0 ... -> T)
                  (C_0 ... -> C) ...)
               (get Σ A_f))
        β•)   
   
   (--> ((@ L (& A_f) (& A_V) ...) Σ)
        ((havoc TC ... V) Σ)        
        (where (• (TC_0 ... -> TC_n) ...)
               (get Σ A_f))
        (where (V_0 ...)
               ((get Σ A_V) ...))
        (where (_ ... (V TC ...) _ ...)
               ,(transpose (term ((V_0 ...)
                                  (TC_0 ...)
                                  ...))))
        havoc)
   
   (--> ((@ L (& A_O) (& A_V) ...) Σ) 
        (M Σ)
        (where O (get Σ A_O))
        (where (V ...) ((get Σ A_V) ...))
	(judgment-holds (δσ^ O L (V ...) Σ M))
	δ^)

   
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_0 Σ)
        (where (• nat C ...) (get Σ A_V))
        if•-t)
   
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_1 Σ)
        (where (• nat C ...) (get Σ A_V))
        if•-f)
  
   (--> ((C L_+ L_- C_n ⚖ (& A_V)) Σ)
	((• T C_0 ... C C_1 ...) Σ)
        (where (• T C_0 ... C C_1 ...) (get Σ A_V))
	known)
   
   ;; FIXME: Pointers be in blame terms to see refinements  
   ;; FIXME: not communicating M to true branch
   (--> ((M L_+ L_- C_n ⚖ (& A_V)) Σ)
	((if0 (@ Λ M (& A_V))
              (& A_V) ;(• T C ... M)
              (blame L_+ C_n M (• T C ...) #;(& A_V)))
         Σ)
        (where (• T C ...) (get Σ A_V))
	(side-condition (not (member (term M) (term (C ...)))))
	check-rem)
   
   ;; FIXME: pointers in blame
   (--> ((M L_+ L_- C ⚖ (& A_V)) Σ)
	((if0 (@ Λ M (& A_V)) (& A_V) (blame L_+ C M V #;(& A_V))) Σ)
        (where V (get Σ A_V))
	(side-condition (not (redex-match SCPCFΣ (• T C ...) (term V))))
	?)
   
   (--> (((C_1 ... -> C) L_+ L_- C_n ⚖ (& A_V)) Σ)
	((λ ([X : T] ...)
           (C L_+ L_- C_n ⚖
              (@ Λ (& A_V)
                 (C_1 L_- L_+ C_n ⚖ X) ...)))
         Σ)
        (where (λ ([X : T] ...) M) (get Σ A_V))
	η)
      
   (--> (((C_1 ... -> C) L_+ L_- C_n ⚖ (& A_V)) Σ)
	((λ ([X : T_1] ...)
           (C L_+ L_- C_n ⚖
              (@ Λ (• (T_1 ... -> T) C_v ...)
                 (C_1 L_- L_+ C_n ⚖ X) ...)))
         Σ)
        (where (• (T_1 ... -> T) C_v ...) (get Σ A_V))
        (where (X ...) ,(map (λ (_) (gensym)) (term (T_1 ...))))
        η•)))


(define-judgment-form SCPCFΣ
  #:mode (δσ^ I I I I O)
  #:contract (δσ^ O L (V ...) Σ M)

  [(δσ^ quotient L (N (• nat C ...)) Σ (• nat pos?))
   (side-condition (¬∈ N 0))]
  [(δσ^ quotient L (0 (• nat C ...)) Σ 0)]
  [(δσ^ quotient L (any (• nat C_0 ... pos? C_1 ...)) Σ (• nat))]
  [(δσ^ quotient L (any (• nat C ...)) Σ (err L nat "Divide by zero"))
   (side-condition (¬∈ pos? C ...))]
  [(δσ^ quotient L ((• nat C ...) 0) Σ (err L nat "Divide by zero"))]
  [(δσ^ quotient L ((• nat C ...) N) Σ (• nat))
   (side-condition (¬∈ N 0))]

  [(δσ^ pos? L ((• nat C_1 ... pos? C_2 ...)) Σ 0)]
  [(δσ^ pos? L ((• nat C ...)) Σ (• nat))
   (side-condition (no-pos? C ...))]

  [(δσ^ zero? L ((• nat C_1 ... zero? C_2 ...)) Σ 0)]
  [(δσ^ zero? L ((• nat C_1 ... pos? C_2 ...)) Σ 1)]
  [(δσ^ zero? L ((• nat C ...)) Σ (• nat))
   (side-condition (¬∈ zero? C ...))
   (side-condition (¬∈ pos? C ...))]

  [(δσ^ add1 L ((• nat C_1 ...)) Σ (• nat pos?))]

  [(δσ^ O L (any_0 ... (• nat C ...) any_1 ...) Σ (• nat))
   (side-condition (¬∈ O quotient zero? pos? add1))])


(define -->scvσ 
  (union-reduction-relations (liftσ SCPCFΣ scvσ)
                             (extend-reduction-relation con-abortσ SCPCFΣ)
                             (extend-reduction-relation err-abortσ SCPCFΣ)))
