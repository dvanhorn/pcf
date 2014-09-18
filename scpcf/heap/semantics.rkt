#lang racket
(provide scvσ -->scvσ ∅ scfoldσ injσ)
(require redex/reduction-semantics 
         pcf/heap/semantics
         cpcf/heap/semantics
         pcf/private/subst
         pcf/private/prover
         (except-in scpcf/semantics δ^)
         scpcf/heap/syntax)


(define-metafunction/extension alloc SCPCFΣ
  scalloc : any_MΣ -> any_A
  [(scalloc ((• L T C ...) any_Σ)) ,(- (term L))])
  

(define -->scv&
  (reduction-relation
   SCPCFΣ #:domain (M Σ)
   (--> ((in-hole E V) Σ) 
        ((in-hole E (& A)) (put Σ A V))
        (where A (scalloc (V Σ)))
        &)))

(define-metafunction SCPCFΣ 
  [(&* (M_0 Σ_0))
   (M_1 Σ_1)        
   (where ((M_1 Σ_1))
          ,(apply-reduction-relation* -->scv& (term (M_0 Σ_0))))])
   
(define -->scvσ1
  (reduction-relation
   SCPCFΣ #:domain (M Σ)
   (--> (M_0 Σ_0)
        (M_2 Σ_2)
        (where (M_1 Σ_1) (&* (M_0 Σ_0)))
        (where (_ ... (any_name (M_2 Σ_2)) _ ...)
               ,(apply-reduction-relation/tag-with-names (liftσ SCPCFΣ scvσ) (term (M_1 Σ_1))))
        (computed-name (term any_name)))
   ; Below is a hack for when &* leaves no redex left in the expression
   ; e.g. • ↦ &1
   (--> (M_0 Σ_0) (M_1 Σ_1)
        (where (M_1 Σ_1) (&* (M_0 Σ_0)))
        (where () ,(apply-reduction-relation (liftσ SCPCFΣ scvσ) (term (M_1 Σ_1))))
        (side-condition (not (equal? (term (M_0 Σ_0)) (term (M_1 Σ_1)))))
        alloc)))



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
   
   (--> ((@ L (& A_f) (& A_x) ...) Σ) 
        ((& A_y) Σ_1)
        (where ((T_0 ... -> T) _ ...) (get Σ A_f))
        (where A_y (alloc ((• T) Σ)))
        (where Fin (fin (T_0 ... -> T) (A_x ... ↦ A_y)))
        (where Σ_1 (put Σ
                        A_y (• T)
                        A_f Fin))
        β•)
   (--> ((@ L (& A_f) (& A) ...) Σ)
        ((& A_y) Σ)
        (where (Fin) (get Σ A_f))
        (where A_y (fin@ Fin A ...))
        (side-condition (term A_y))
        fin@)
   (--> ((@ L (& A_f) (& A) ...) Σ)
        ((& A_y) Σ_1)
        (where ((name Fin (fin (_ ... -> T) _ ...)))
               (get Σ A_f))
        (where #f (fin@ Fin A ...))
        (where A_y (alloc ((• T) Σ)))
        (where Σ_1 (put Σ
                        A_y (• T)
                        A_f (fin+ Fin (A ... ↦ A_y))))
        fin+)
   
   (--> ((@ L (& A_f) (& A_V) ...) Σ)
        ((@ L (& A_f) (& A_V) ...) Σ_1)
        ; partially concretize the unknown function
        (where ((T_dom ... -> _) _ ...)
               (get Σ A_f))
        (judgment-holds (havoc/n Λ (T_dom ...) V_hv))
        (where Σ_1 (put Σ A_f V_hv))
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

(define fresh!
  (let ([next 100])
    (λ () (begin (set! next (+ 1 next)) next))))

;; Nondeterministically generate a function that havocs one of its argument
;; E.g. (λ ([f₁ : τ₁] ... [f : τ] [f₂ : τ₂]...) (havoc f))
(define-judgment-form SCPCFΣ
  #:contract (havoc/n L (T ...) V)
  #:mode     (havoc/n I I       O)
  [(where (T_x ... -> T_y) T)
   (where M (havocᵢ ,(fresh!) L T f))
   (where (X_l ...)
          ,(variables-not-in (term M) (make-list (length (term (T_l ...))) 'l)))
   (where (X_r ...)
          ,(variables-not-in (term M) (make-list (length (term (T_r ...))) 'r)))
   -----------------------------------------------------------------------------
   (havoc/n L (T_l ... T T_r ...)
            (λ ([X_l : T_l] ... [f : T] [X_r : T_r] ...) M))])

;; Synthesize havoc-ing expression for type `T`
(define-metafunction SCPCFΣ
  ; The first argument is a hack around Redex's memoization
  havocᵢ : _ L T M -> M
  [(havocᵢ _ L nat M) M]
  [(havocᵢ _ L (T_x ... -> T_y) M)
   (havocᵢ ,(fresh!) L T_y (@ L M (• ,(fresh!) T_x) ...))])

(define-metafunction SCPCFΣ
  eq : M -> M
  [(eq M)
   (λ ([x : nat]) (@ Λ = x M))])

(define-metafunction SCPCFΣ
  neq : M -> M
  [(neq M)
   (λ ([x : nat]) (@ Λ not (@ Λ = x M)))])

(define-metafunction SCPCFΣ
  ≡/c : O A ... -> M
  [(≡/c O A ...)
   (λ ([x : nat]) (@ Λ = x (@ Λ O (& A) ...)))])
   

(define-judgment-form SCPCFΣ
  #:mode (δσ^ I I I I O O)
  #:contract (δσ^ O L (A ...) Σ (M Σ) any)

  ;; Requires more primitives (=, etc.)
  [(δσ^ ÷ L (A_n A_d) Σ ((• nat (≡/c / A_n A_d)) (refine Σ A_d pos?)) δ-÷1)
   (side-condition ,(member (term (⊢ Σ A_d zero?)) '(? ✗)))]
  
  [(δσ^ ÷ L (A_n A_d) Σ ((err L nat "Divide by zero") (refine Σ A_d zero?)) δ-÷2)
   (side-condition ,(member (term (⊢ Σ A_d zero?)) '(? ✓)))]

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
  [(δσ^ O L (A_0 ... A A_1 ...) Σ ((• nat (≡/c O A_0 ... A A_1 ...)) Σ) RULE)
   (where (nat C ...) (get Σ A))
   (side-condition (¬∈ O quotient / zero? pos? add1 =))])


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
  (union-reduction-relations -->scvσ1
                             (extend-reduction-relation con-abortσ SCPCFΣ)
                             (extend-reduction-relation err-abortσ SCPCFΣ)))

;; HACK: Attach counterexample to folded result
(define-metafunction SCPCFΣ
  scfoldσ : (M Σ) -> any
  [(scfoldσ (M Σ))
   ((scfoldσ₁ (M Σ_1)) with any_ctx)
   (side-condition (or (redex-match? SCPCFΣ (err _ ...) (term M))
                       (redex-match? SCPCFΣ (blame _ ...) (term M))))
   ; If model generating is successful, `Σ_1` is one possible breaking context
   (where any_model (z3/model Σ))
   (side-condition (term any_model))
   (where Σ_1 ,(for/fold ([Σᵢ (term Σ)]) ([(a n) (term any_model)])
                 (term (put ,Σᵢ ,a ,n))))
   (where any_ctx ,(for/hash ([(a M) (term Σ_1)] #:when (<= -99 a -1))
                     (values a (term (scfoldσ₁ ((& ,a) Σ_1))))))]
  [(scfoldσ any) (scfoldσ₁ any)])

(define-metafunction/extension cfoldσ SCPCFΣ
  scfoldσ₁ : (M Σ) -> M 
  [(scfoldσ₁ ((& A) Σ))
   (scfoldσ₁ (M Σ))
   (where (M C ...) (get Σ A))]
  [(scfoldσ₁ ((& A) Σ))
   (scfoldσ₁ ((• T C ...) Σ))
   (where (T C ...) (get Σ A))]
  [(scfoldσ₁ ((• natural T C ...) Σ))
   (scfoldσ₁ ((& ,(- (term natural))) Σ))]
  [(scfoldσ₁ ((• _ ... zero? _ ...) _)) 0]
  [(scfoldσ₁ ((• T C ...) Σ)) (• T C ...)]
  [(scfoldσ₁ ((• L T C ...) Σ)) (• L T C ...)]
  [(scfoldσ₁ ((fin (T_x ... -> T) (A_x ... ↦ A) ...) Σ))
   (λ ([X : T_x] ...)
     (scfoldσ₁ ((COND [(=* (X ...) ((& A_x) ...)) (& A)] ... #:else 42) Σ)))
   (where (X ...)
          ,(variables-not-in (term '()) (make-list (length (term (T_x ...))) 'n)))]
  ;[(scfoldσ₁ ((M ...) Σ)) ((scfoldσ₁ M) ...)]
  [(scfoldσ₁ (Ω Σ)) Ω])

;;;;; Macros

(define-metafunction SCPCFΣ
  COND : [M M] ... #:else M -> M
  [(COND #:else M) M]
  [(COND [0 M] _ ...) M]
  [(COND [M_1 M_2] any ...) (if0 M_1 M_2 (COND any ...))])

(define-metafunction SCPCFΣ
  =* : [M ..._1] [M ..._1] -> M
  [(=* (M_1 ...) (M_2 ...)) (AND (@ 'Λ #|easy fix, hack for now|# = M_1 M_2) ...)])

(define-metafunction SCPCFΣ
  AND : M ... -> M
  [(AND) 0] ; 0 = true
  [(AND M) M]
  [(AND M_1 M ...) (if0 M_1 (AND M ...) 1)])
