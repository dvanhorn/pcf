#lang racket
(provide scvσ -->scvσ ∅ scfoldσ injσ)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/semantics
         pcf/private/subst
         pcf/private/prover
         (except-in scpcf/semantics δ^)
         scpcf/heap/syntax)

(define scvσ
  (extend-reduction-relation cvσ ;; maybe cvσ?
   SCPCFΣ #:domain (M Σ)
   
   (--> ((@ L (& A_f) P_V ..._1) Σ)
        ((subst (X P_V) ... M) Σ)
        (where ((λ ([X : T] ..._1) M) C ...)
               (get Σ A_f))
        β)
   
   (--> (Ω Σ) (Ω Σ) Ω) ;; Loop

   (--> ((μ (X : T) V) Σ)
        ((subst (X (& A)) V) (put Σ A (T)))
        (where A (alloc ((μ (X : T) V) Σ)))
        μ) ;; μ^
     
   (--> ((@ L (& A_f) P ...) Σ)
        ((• T C ...) Σ)        
        (where ((T_0 ... -> T)
                (C_0 ... -> C) ...)
               (get Σ A_f))
        β•)

   (--> ((@ L (& A_p) (& A_a)) Σ)
        (0 Σ)
        (where (M_p) (get Σ A_p))   ; may cause a type error if M_p is not in C.     
        (where ✓ (⊢ Σ A_a M_p))   ; but unsound(?) to do for arbitrary C?
        known-pred-holds)
   
   (--> ((@ L (& A_p) (& A_a)) Σ)
        (1 Σ)
        (where (M_p) (get Σ A_p))   ; may cause a type error if M_p is not in C.     
        (where ✗ (⊢ Σ A_a M_p))   ; but unsound(?) to do for arbitrary C?
        known-pred-doesnt-hold)
   
   (--> ((@ L (& A_f) (& A_V) ...) Σ)
        ((havoc TC ... V) Σ)        
        (where ((TC_0 ... -> TC_n) ...)
               (get Σ A_f))
        (where ((V_0) ...)            ;; Ugh.  WTF to do here?
               ((get Σ A_V) ...))
        (where (_ ... (V TC ...) _ ...)
               ,(transpose (term ((V_0 ...)
                                  (TC_0 ...)
                                  ...))))
        havoc)
   
   (--> ((@ L (& A_O) (& A_V) ...) Σ) 
        (M Σ_1)
        (where (O C ...) (get Σ A_O))  ;; Maybe change δ to take a storable
        (judgment-holds (δσ^ O L (A_V ...) Σ (M Σ_1) any_rule))
	(computed-name (term any_rule)))
      
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_0 Σ_1)
        (judgment-holds (δσ^ zero? Λ (A_V) Σ (0 Σ_1) any))
        if0-t)
   (--> ((if0 (& A_V) M_0 M_1) Σ)
        (M_1 Σ_1)
        (judgment-holds (δσ^ zero? Λ (A_V) Σ (1 Σ_1) any))
        if0-f)
      
   (--> (((C_1 ... -> C) L_+ L_- C_n ⚖ (& A_V)) Σ)
	((λ ([X : T_1] ...)
           (C L_+ L_- C_n ⚖
              (@ Λ (& A_v)
                 (C_1 L_- L_+ C_n ⚖ X) ...)))
         Σ)
        (where ((T_1 ... -> T) C_v ...) (get Σ A_V))
        (where (X ...) ,(map (λ (_) (gensym)) (term (T_1 ...))))
        η•)))

(define-metafunction SCPCFΣ
  eq : M -> M
  [(eq M)
   (λ ([x : nat]) (@ Λ = x M))])

(define-metafunction SCPCFΣ
  neq : M -> M
  [(neq M)
   (λ ([x : nat]) (@ Λ not (@ Λ = x M)))])
   

(define-judgment-form SCPCFΣ
  #:mode (δσ^ I I I I O O)
  #:contract (δσ^ O L (A ...) Σ (M Σ) any)

  ;; FIXME: Add equation between result and inputs
  ;; Requires more primitives (=, etc.)
  [(δσ^ quotient L (A_n A_d) Σ ((• nat) (refine Σ A_d pos?)) δ-quotient1)
   (where (N) (get Σ A_n))
   (where (nat C ...) (get Σ A_d))  
   (side-condition (¬∈ N 0))]
      
  [(δσ^ quotient L (A_n A_d) Σ (0 (refine Σ A_d pos?)) δ-quotient2)
   (where (0) (get Σ A_n))
   (where (nat C ...) (get Σ A_d))]
  
  [(δσ^ quotient L (any A_d) Σ ((• nat) Σ) δ-quotient3)
   (where (nat C_0 ... pos? C_1 ...) (get Σ A_d))]
  
  [(δσ^ quotient L (any A_d) Σ 
        ((err L nat "Divide by zero") 
         (refine Σ A_d zero?)) δ-quotient4)
   (where (nat C ...) (get Σ A_d))        
   (side-condition (¬∈ pos? C ...))]
  
  [(δσ^ quotient L (A_n A_d) Σ ((err L nat "Divide by zero") Σ) δ-quotient5)
   (where (nat C ...) (get Σ A_n))
   (where (0) (get Σ A_d))]
  
  [(δσ^ quotient L (A_n A_d) Σ ((• nat) Σ) δ-quotient6)
   (where (nat C ...) (get Σ A_n))
   (where (N) (get Σ A_d))
   (side-condition (¬∈ N 0))]

  [(δσ^ pos? L (A) Σ (0 Σ) δ-pos?✓)
   (where ✓ (⊢ Σ A pos?))]
  [(δσ^ pos? L (A) Σ (1 Σ) δ-pos?✗)
   (where ✗ (⊢ Σ A pos?))]
  [(δσ^ pos? L (A) Σ (0 (refine Σ A pos?)) δ-pos??1)
   (where ? (⊢ Σ A pos?))]  
  [(δσ^ pos? L (A) Σ (1 (refine Σ A zero?)) δ-pos??2)
   (where ? (⊢ Σ A pos?))]

  [(δσ^ zero? L (A) Σ (0 Σ) δ-zero?✓)
   (where ✓ (⊢ Σ A zero?))]  
  [(δσ^ zero? L (A) Σ (1 Σ) δ-zero?✗)
   (where ✗ (⊢ Σ A zero?))]  
  [(δσ^ zero? L (A) Σ (0 (refine Σ A zero?)) δ-zero??1)
   (where ? (⊢ Σ A zero?))] 
  [(δσ^ zero? L (A) Σ (1 (refine Σ A pos?)) δ-zero??2)
   (where ? (⊢ Σ A zero?))]

  [(δσ^ add1 L (A) Σ ((• nat pos?) Σ) δ-add1)
   (where (nat C ...) (get Σ A))]
  
  [(δσ^ = L (A_0 A_1) Σ (0 Σ) δ-=✓)
   (where ✓ (⊢ Σ A_0 (eq (& A_1))))]  
  [(δσ^ = L (A_0 A_1) Σ (1 Σ) δ-=✗)
   (where ✗ (⊢ Σ A_0 (eq (& A_1))))]
  [(δσ^ = L (A_0 A_1) Σ (0 (refine Σ A_1 (eq (& A_0)))) δ-=?1) 
   (where ? (⊢ Σ A_0 (eq (& A_1))))]
  [(δσ^ = L (A_0 A_1) Σ (1 (refine Σ A_1 (neq (& A_0)))) δ-=?2)
   (where ? (⊢ Σ A_0 (eq (& A_1))))]
  
  ;; FIXME: This rule probably needs to be treated more precisely
  [(δσ^ O L (any_0 ... A any_1 ...) Σ ((• nat) Σ) RULE)
   (where (nat C ...) (get Σ A))
   (side-condition (¬∈ O quotient zero? pos? add1 =))])


;; FIXME: should have refinements of concrete values too

(define-metafunction SCPCFΣ
  refine : Σ A C ... -> Σ
  [(refine Σ A C ...)
   (put Σ A (T C_1 ...))
   (where (T C_0 ...) (get Σ A))
   (where (C_1 ...)
          (join-contracts C ... C_0 ...))]
  [(refine Σ A C ...)
   (put Σ A (V C_1 ...))
   (where (V C_0 ...) (get Σ A))
   (where (C_1 ...)
          (join-contracts C ... C_0 ...))])

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
  [(scfoldσ ((& A) Σ))
   (scfoldσ (M Σ))
   (where (M C ...) (get Σ A))]
  [(scfoldσ ((& A) Σ))
   (scfoldσ ((• T C ...) Σ))
   (where (T C ...) (get Σ A))]
  [(scfoldσ ((• T C ...) Σ)) (• T C ...)]
  ;[(scfoldσ ((M ...) Σ)) ((scfoldσ M) ...)]
  ;[(scfoldσ (M Σ)) M])
  [(scfoldσ (Ω Σ)) Ω])
