#lang racket
(provide scvσ -->scvσ ∅ scfoldσ injσ)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/semantics
         pcf/private/subst
         (except-in scpcf/semantics δ^)
         scpcf/heap/syntax)

(define scvσ
  (extend-reduction-relation cvσ ;; maybe cvσ?
   SCPCFΣ #:domain (M Σ)
   
   (--> ((@ L (& A_f) P_V ..._1) Σ)
        ((subst (X P_V) ... M) Σ)
        (where (λ ([X : T] ..._1) M)
               (get Σ A_f))
        β)
   
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
        (M Σ_1)
        (where O (get Σ A_O))
        (judgment-holds (δσ^ O L (A_V ...) Σ (M Σ_1)))
	δ^)
      
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_0 Σ_1)
        (judgment-holds (δσ^ zero? Λ (A_V) Σ (0 Σ_1)))       
        if•-t)
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_1 Σ_1)
        (judgment-holds (δσ^ zero? Λ (A_V) Σ (1 Σ_1)))
        if•-f)   
  
   (--> ((C L_+ L_- C_n ⚖ (& A_V)) Σ)
	((• T C_0 ... C C_1 ...) Σ)
        (where (• T C_0 ... C C_1 ...) (get Σ A_V))
	known)
   
   (--> ((M L_+ L_- C_n ⚖ (& A_V)) Σ)
	((if0 (@ Λ M (& A_V))
              (& A_V)
              (blame L_+ C_n M (& A_V)))
         Σ)
        (where (• T C ...) (get Σ A_V))
        (where #t (¬∈ M C ...))
        check)
   
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
              (@ Λ (& A_v)
                 (C_1 L_- L_+ C_n ⚖ X) ...)))
         Σ)
        (where (• (T_1 ... -> T) C_v ...) (get Σ A_V))
        (where (X ...) ,(map (λ (_) (gensym)) (term (T_1 ...))))
        η•)))

(define-metafunction SCPCFΣ
  ⊢ : Σ A C -> proves or refutes or abstain
  [(⊢ Σ A C)
   '..])


(define-judgment-form SCPCFΣ
  #:mode (δσ^ I I I I O)
  #:contract (δσ^ O L (A ...) Σ (M Σ))

  ;; FIXME: Add equation between result and inputs
  ;; Requires more primitives (=, etc.)
  [(δσ^ quotient L (A_n A_d) Σ ((• nat) (refine Σ A_d pos?)))
   (where N (get Σ A_n))
   (where (• nat C ...) (get Σ A_d))  
   (side-condition (¬∈ N 0))]
      
  [(δσ^ quotient L (A_n A_d) Σ (0 (refine Σ A_d pos?)))
   (where 0 (get Σ A_n))
   (where (• nat C ...) (get Σ A_d))]
  
  [(δσ^ quotient L (any A_d) Σ ((• nat) Σ))
   (where (• nat C_0 ... pos? C_1 ...) (get Σ A_d))]
  
  [(δσ^ quotient L (any A_d) Σ 
        ((err L nat "Divide by zero") 
         (refine Σ A_d zero?)))
   (where (• nat C ...) (get Σ A_d))        
   (side-condition (¬∈ pos? C ...))]
  
  [(δσ^ quotient L (A_n A_d) Σ ((err L nat "Divide by zero") Σ))
   (where (• nat C ...) (get Σ A_n))
   (where 0 (get Σ A_d))]
  
  [(δσ^ quotient L (A_n A_d) Σ ((• nat) Σ))
   (where (• nat C ...) (get Σ A_n))
   (where N (get Σ A_d))
   (side-condition (¬∈ N 0))]

  [(δσ^ pos? L (A) Σ (0 Σ))
   (where (• nat C_1 ... pos? C_2 ...) (get Σ A))]
  
  [(δσ^ pos? L (A) Σ (0 (refine Σ A pos?)))
   (where (• nat C ...) (get Σ A))
   (side-condition (no-pos? C ...))]
  [(δσ^ pos? L (A) Σ (1 (refine Σ A zero?)))
   (where (• nat C ...) (get Σ A))
   (side-condition (no-pos? C ...))]

  [(δσ^ zero? L (A) Σ (0 Σ))
   (where (• nat C_1 ... zero? C_2 ...) (get Σ A))]  
  [(δσ^ zero? L (A) Σ (1 Σ))
   (where (• nat C_1 ... pos? C_2 ...) (get Σ A))]
  
  [(δσ^ zero? L (A) Σ (0 (refine Σ A zero?)))
   (where (• nat C ...) (get Σ A))
   (side-condition (¬∈ zero? C ...))
   (side-condition (¬∈ pos? C ...))]
  [(δσ^ zero? L (A) Σ (1 (refine Σ A pos?)))
   (where (• nat C ...) (get Σ A))
   (side-condition (¬∈ zero? C ...))
   (side-condition (¬∈ pos? C ...))]

  [(δσ^ add1 L (A) Σ ((• nat pos?) Σ))
   (where (• nat C ...) (get Σ A))]

  ;; (= • N)
  [(δσ^ = L (A_0 A_1) Σ 
        (0 (refine Σ A_0 (λ ([x : nat]) (@ Λ = x (& A_1))))))
   (where (• nat C ...) (get Σ A_0))
   (where N (get Σ A_1))]
  ;; FIXME: refine not equal
  [(δσ^ = L (A_0 A_1) Σ (1 Σ))
   (where (• nat C ...) (get Σ A_0))
   (where N (get Σ A_1))]
  
  ;; (= N •)
  [(δσ^ = L (A_0 A_1) Σ 
        (0 (refine Σ A_1  (λ ([x : nat]) (@ Λ = x (& A_0))))))
   (where N (get Σ A_0))
   (where (• nat C ...) (get Σ A_1))]
  ;; FIXME: refine not equal
  [(δσ^ = L (A_0 A_1) Σ (1 Σ))  
   (where N (get Σ A_0))
   (where (• nat C ...) (get Σ A_1))]
    
  ;; FIXME: need a more general mechanism for proving, disproving =.
  ;; (= • •) 
  [(δσ^ = L (A_0 A_1) Σ 
        (0 (refine Σ A_0 (λ ([x : nat]) (@ Λ = x (& A_1))))))
   (where (• nat C_0 ...) (get Σ A_0))
   (where (• nat C_1 ...) (get Σ A_1))]
  [(δσ^ = L (A_0 A_1) Σ 
        (1 (refine Σ A_0 (λ ([x : nat]) (@ Λ not (@ Λ = x (& A_1)))))))
   (where (• nat C_0 ...) (get Σ A_0))
   (where (• nat C_1 ...) (get Σ A_1))]
   
  
  ;; FIXME: This rule probably needs to be treated more precisely
  [(δσ^ O L (any_0 ... A any_1 ...) Σ ((• nat) Σ))
   (where (• nat C ...) (get Σ A))
   (side-condition (¬∈ O quotient zero? pos? add1 =))])


;; FIXME: should have refinements of concrete values too

(define-metafunction SCPCFΣ
  refine : Σ A C ... -> Σ
  [(refine Σ A C ...)
   (put Σ A (• T C_1 ...))   
   (where (• T C_0 ...) (get Σ A))
   (where (C_1 ...)
          (join-contracts C ... C_0 ...))]

  ;; Change here to refine concrete values
  [(refine Σ A C ...) Σ])

(define-metafunction SCPCFΣ
  join-contracts : C ... -> (C ...)
  [(join-contracts C ...)
   ,(set->list (apply set (term (C ...))))])


(define -->scvσ 
  (union-reduction-relations (liftσ SCPCFΣ scvσ)
                             (extend-reduction-relation con-abortσ SCPCFΣ)
                             (extend-reduction-relation err-abortσ SCPCFΣ)))

(define-metafunction/extension cfoldσ SCPCFΣ
  scfoldσ : (M Σ) -> M 
  [(scfoldσ ((• T C ...) Σ)) (• T C ...)]
  ;[(scfoldσ ((M ...) Σ)) ((scfoldσ M) ...)]
  ;[(scfoldσ (M Σ)) M])
  [(scfoldσ (Ω Σ)) Ω])
