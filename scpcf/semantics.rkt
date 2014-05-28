#lang racket
(require redex/reduction-semantics
	 pcf/semantics
	 pcf/private/subst
	 cpcf/semantics
	 scpcf/syntax)
(provide sc -->scv)

(define sc
  (extend-reduction-relation v
   SCPCF #:domain M
   (--> Ω Ω Ω) ;; Loop
      
   (--> (μ (X : T) S) (subst (X (• T)) S) μ) ;; μ^

   (--> (@ L (• (T_0 ... -> T)
		(C_0 ... -> C) ...)
	   V ...)
	(• T C ...) β•)

   #;(--> (@ L (• (T_0 ..._1 T T_1 ... -> T_o)  ;; FIXME bug in _1 in Redex
		  (C_0 ..._1 C C_1 ... -> C_o)
		  ...)
	     V_0 ..._1 V V_1 ...)
	  (havoc T C ... V)
	  havoc)
   ;; Unary case of above
   (-->  (@ L (• (T -> T_o)  ;; FIXME bug in _1 in Redex
		 (C -> C_o)
		 ...)
	    V)
	  (havoc T C ... V)
	  havoc)

   (--> (@ L O V ...) M
	(judgment-holds (δ^ O L (V ...) M))
	δ^)
   (--> (if0 (• nat C ...) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat C ...) M_0 M_1) M_1 if•-f)

   (--> (C L_+ L_- C_n ⚖ (• T C_0 ... C C_1 ...))
	(• T C_0 ... C C_1 ...)
	known)

   (--> (M L_+ L_- C_n ⚖ (• T C ...))
	(if0 (@ 'Λ M (• T C ...))
	     (• T C ... M)
	     (blame L_+ C_n M (• T C ...)))
	(side-condition (not (member (term M) (term (C ...)))))
	check-rem)

   (--> (M L_+ L_- C ⚖ V)
	(if0 (@ 'Λ M V) V (blame L_+ C M V))
	(side-condition (not (redex-match SCPCF (• T C ...) (term V))))
	?)
   
   (--> ((C_1 ... -> C) L_+ L_- C_n ⚖ (λ ([X : T] ...) M))
	(λ ([X : T] ...)
	  (C L_+ L_- C_n ⚖
	     (@ 'Λ (λ ([X : T] ...) M)
		(C_1 L_- L_+ C_n ⚖ X) ...)))
	η)

   (--> ((C_1 ... -> C) L_+ L_- C_n ⚖ (• (T_1 ... -> T) C_v ...))
	(λ ([X : T_1] ...)
	  (C L_+ L_- C_n ⚖
	     (@ 'Λ (• (T_1 ... -> T) C_v ...)
		(C_1 L_- L_+ C_n ⚖ X) ...)))
	(where (X ...) ,(map (λ (_) (gensym)) (term (T_1 ...))))
	η•)))

(define-metafunction SCPCF
  ¬∈ : any any ... -> #t or #f
  [(¬∈ any any_1 ... any any_2 ...) #f]
  [(¬∈ any any_1 ...) #t])


(define-judgment-form SCPCF
  #:mode (δ^ I I I O)
  #:contract (δ^ O L (V ...) M)

  [(δ^ quotient L (N (• nat C ...)) (• nat pos?))
   (side-condition (¬∈ N 0))]
  [(δ^ quotient L (0 (• nat C ...)) 0)]


  [(δ^ quotient L (any (• nat C_0 ... pos? C_1 ...)) (• nat))]
  [(δ^ quotient L (any (• nat C ...)) (err L nat "Divide by zero"))
   (side-condition (¬∈ pos? C ...))]
  [(δ^ quotient L ((• nat C ...) 0)   (err L nat "Divide by zero"))]
  [(δ^ quotient L ((• nat C ...) N)   (• nat))
   (side-condition (¬∈ N 0))]

  [(δ^ pos? L ((• nat C_1 ... pos? C_2 ...)) 0)]
  [(δ^ pos? L ((• nat C ...)) (• nat))
   (side-condition (no-pos? C ...))]

  [(δ^ zero? L ((• nat C_1 ... zero? C_2 ...)) 0)]
  [(δ^ zero? L ((• nat C_1 ... pos? C_2 ...)) 1)]
  [(δ^ zero? L ((• nat C ...)) (• nat))
   (side-condition (¬∈ zero? C ...))
   (side-condition (¬∈ pos? C ...))]

  [(δ^ add1 L ((• nat C_1 ...)) (• nat pos?))]

  [(δ^ O L (any_0 ... (• nat C ...) any_1 ...) (• nat))
   (side-condition (¬∈ O quotient zero? pos? add1))])

(define-metafunction SCPCF
  havoc : T C ... M -> M
  [(havoc nat C ... M) (@ Λ (λ ([y : nat]) Ω) M)]
  [(havoc (T_0 -> T_1) (C_0 -> C_1) ... M)
   (havoc T_1 C_1 ... (@ 'Λ M (• T_0 C_0 ...)))])

(define -->scv
  (union-reduction-relations (context-closure sc SCPCF E)
			     (extend-reduction-relation err-abort SCPCF)
			     (extend-reduction-relation con-abort SCPCF)))
