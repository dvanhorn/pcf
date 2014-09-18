#lang racket
(provide vσ err-abortσ -->vσ alloc put get get-v ∅ liftσ not-mt? injσ foldσ)
(require pcf/heap/syntax
	 pcf/semantics
	 pcf/private/subst
	 redex/reduction-semantics)

(define (injσ M) (list M (term ∅)))

(define -->v&
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> ((in-hole E V) Σ)
	((in-hole E (& A)) (put Σ A V))
	(where A (alloc (V Σ)))
	&)))

(define-metafunction PCFΣ
  [(&* (M_0 Σ_0))
   (M_1 Σ_1)
   (where ((M_1 Σ_1))
	  ,(apply-reduction-relation* -->v& (term (M_0 Σ_0))))])

(define -->vσ1
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> (M_0 Σ_0)
	(M_1 Σ_1)
	(where (M_2 Σ_2) (&* (M_0 Σ_0)))
	(where (_ ... (any_name (M_1 Σ_1)) _ ...)
	       ,(apply-reduction-relation/tag-with-names (liftσ PCFΣ vσ) (term (M_2 Σ_2))))
	(computed-name (term any_name)))))



(define vσ
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> (V Σ) ((& A) (put Σ A V))
	(where A (alloc (V Σ)))
	&)
   (--> ((@ L (& A_f) P_V ..._1) Σ)
	((subst (X P_V) ... M) Σ)
	(where ((λ ([X : T] ..._1) M))
	       (get Σ A_f))
	β)
   (--> ((μ (X : T) V) Σ)
	((& A) (put Σ A (subst (X (& A)) V)))
	(where A (alloc ((μ (X : T) V) Σ)))
	μ)
   (--> ((@ L (& A_O) (& A_V) ...) Σ)
	(M Σ)
	(where (O) (get Σ A_O))
	(where ((V) ...) ((get Σ A_V) ...))
	(judgment-holds (δ O L (V ...) M))
	δ)
   (--> ((if0 (& A) M_0 M_1) Σ)
	(M_0 Σ)
	(where (0) (get Σ A))
	if0-t)
   (--> ((if0 (& A) M_0 M_1) Σ)
	(M_1 Σ)
	(where (N) (get Σ A))
	(judgment-holds (nonzero? N))
	if0-f)))


;; Like (context-closure L r (E Σ))
;; but r is a relation on M × Σ.
(define-syntax-rule (liftσ L r)
  (reduction-relation
   L #:domain (M Σ)
   (--> ((in-hole E M) Σ)
	((in-hole E M_1) Σ_1)
	(where (_ (... ...) (any_n (M_1 Σ_1)) _ (... ...))
	       ,(apply-reduction-relation/tag-with-names r (term (M Σ))))
	(computed-name (term any_n)))))

(define err-abortσ
  (reduction-relation
   PCFΣ #:domain (M Σ)
   (--> ((in-hole E (err L T string)) Σ)
	((err L T string) Σ)
	(where #t (not-mt? E))
	err-abort)))

(define -->vσ
  (union-reduction-relations -->vσ1 err-abortσ))


(define-metafunction PCFΣ
  alloc : any_MΣ -> any_A
  [(alloc (any_M any_Σ))
   ,(add1 (apply max 0 (hash-keys (term any_Σ))))])


(define-metafunction PCFΣ
  get-v : any_Σ any_A -> any_S
  [(get-v any_Σ any_A)
   ,(let ((r (hash-ref (term any_Σ) (term any_A))))
      (match r
	[(list (? (redex-match? PCFΣ T) t) cs) `(• ,t ,@(set->list cs))]
	[(list v cs) v]))])

(define-metafunction PCFΣ
  get : any_Σ any_A -> any_S
  [(get any_Σ any_A)
   ,(let ((r (hash-ref (term any_Σ) (term any_A))))
      (match r
	[(list t cs) `(,t ,@(set->list cs))]
	[(list v cs) `(,v ,@(set->list cs))]))])

(define-metafunction PCFΣ
  ;put : any_Σ any_A any_V ... ... -> any_Σ
  [(put any_Σ) any_Σ]
  [(put any_Σ any_A any_V any ...) (put (put₁ any_Σ any_A any_V) any ...)])

(define-metafunction PCFΣ
  ;put₁ : any_Σ any_A any_V -> any_Σ
  [(put₁ any_Σ any_A (any_0 ... -> any)) ; hack on top of hack
   ,(hash-set (term any_Σ) (term any_A) (list (term (any_0 ... -> any)) (set)))]
  [(put₁ any_Σ any_A (λ any ...)) ; hack on top of hack
   ,(hash-set (term any_Σ) (term any_A) (list (term (λ any ...)) (set)))]
  [(put₁ any_Σ any_A V)
   ,(hash-set (term any_Σ) (term any_A) (list (term V) (set)))]
  [(put₁ any_Σ any_A (• T any_C ...))
   ,(hash-set (term any_Σ) (term any_A) (list (term T) (apply set (term (any_C ...)))))]
  [(put₁ any_Σ any_A (• L T any_C ...))
   ,(hash-set (term any_Σ) (term any_A) (list (term T) (apply set (term (any_C ...)))))]
  [(put₁ any_Σ any_A (T any_C ...))
   ,(hash-set (term any_Σ) (term any_A) (list (term T) (apply set (term (any_C ...)))))]
  [(put₁ any_Σ any_A any_V)
   ,(hash-set (term any_Σ) (term any_A) (list (term any_V) (set)))])

(define-term ∅ ,(hash))


(define-metafunction PCFΣ
  foldσ : (M Σ) -> M
  [(foldσ (X Σ)) X]
  [(foldσ ((& A) Σ))
   (foldσ (M Σ))
   (where (M) (get Σ A))]
  [(foldσ ((@ L M ...) Σ))
   (@ L (foldσ (M Σ)) ...)]
  [(foldσ ((μ (X : T) V) Σ))
   (μ (X : T) (foldσ (V Σ)))]
  [(foldσ ((if0 M ...) Σ))
   (if0 (foldσ (M Σ)) ...)]
  [(foldσ (N Σ)) N]
  [(foldσ (O Σ)) O]
  [(foldσ ((λ ([X : T] ...) M) Σ))
   (λ ([X : T] ...) (foldσ (M Σ)))]
  [(foldσ ((err L T string) Σ))
   (err L T string)])
