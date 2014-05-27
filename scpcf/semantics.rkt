#lang racket
(require redex/reduction-semantics
         pcf/semantics
         cpcf/semantics
         scpcf/syntax)
(provide sc -->scv)

(define sc
  (reduction-relation
   SCPCF #:domain M
   (--> (@ L (• (T_0 ... -> T)
                (C_0 ... -> C) ...)
           V ...)
	(• T C ...) β•)

   (--> (@ L (• (T -> T_o)  ;; FIXME bug in _1 in Redex
                (C -> C_o) ...)
           V)
	(havoc T C ... V)
	havoc)

   (--> (@ L O V ...) M
	(judgment-holds (δ^ O L (V ...) M))
	δ^)
   (--> (if0 (• nat C ...) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat C ...) M_0 M_1) M_1 if•-f)

   (--> (any? L_+ L_- C ⚖ V) V any?)

   (--> (C L_+ L_- C_n ⚖ (• T C_0 ... C C_1 ...))
	(• T C_0 ... C C_1 ...)
	(side-condition (not (eq? (term M) 'any?)))
	known)

   (--> (M L_+ L_- C_n ⚖ (• T C ...))
	(if0 (@ 'Λ M (• T C ...))
	     (• T C ... M)
	     (blame L_+ C_n M (• T C ...)))
	(side-condition (not (eq? (term M) 'any?)))
	(side-condition (not (member (term M) (term (C ...)))))
	check-rem)

   (--> (M L_+ L_- C ⚖ V)
        (if0 (@ 'Λ M V) V (blame L_+ C M V))
	(side-condition (not (eq? (term M) 'any?)))
	(side-condition (not (redex-match SCPCF (• T C ...) (term V))))
	?)
   (--> ((C_1 ... -> C) L_+ L_- C_n ⚖ (λ ([X : T] ...) M))
	(λ ([X : T] ...)
	  (C L_+ L_- C_n ⚖ (@ 'Λ (λ ([X : T] ...) M) (C_1 L_- L_+ C_n ⚖ X) ...)))
	(side-condition (not (eq? (term C) 'any?)))
	η)
   (--> ((C_1 ... -> any?) L_+ L_- C_n ⚖ (λ ([X : T] ...) M))
	(λ ([X : T] ...)
	  (@ 'Λ (λ ([X : T] ...) M) (C_1 L_- L_+ C_n ⚖ X) ...))
	ηt)))

(define-metafunction SCPCF
  not-op-in? : any O ... -> #t or #f
  [(not-op-in? O any_1 ... O any_2 ...) #f]
  [(not-op-in? any O ...) #t])

(define-metafunction SCPCF
  ¬∈ : any any ... -> #t or #f
  [(¬∈ any any_1 ... any any_2 ...) #f]
  [(¬∈ any any_1 ...) #t])


(define-judgment-form SCPCF
  #:mode (δ^ I I I O)
  #:contract (δ^ O L (V ...) M)
  [(δ^ quotient L (any (• nat C ...)) (• nat))]
  [(δ^ quotient L (any (• nat C_0 ... pos? C_1 ...)) (• nat))]
  [(δ^ quotient L (any (• nat C ...)) (err L nat "Divide by zero"))
   (side-condition (no-pos? C ...))]
  [(δ^ quotient L ((• nat C ...) 0)   (err L nat "Divide by zero"))]
  [(δ^ quotient L ((• nat C ...) N)   (• nat))
   (side-condition (not-zero? N))]
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
  [(havoc nat C ... M) (μ (x : nat) x)]
  [(havoc (T_0 -> T_1) (C_0 -> C_1) ...
	  (λ ((X : T)) (@ 'Λ (λ ((X : T)) M) (C_3 ⚖ X))))
   (havoc T_1 C_1 ...
	  (@ 'Λ (λ ((X : T)) M) (• T_0 C_3 C_0 ...)))]
  [(havoc (T_0 -> T_1) (C_0 -> C_1) ...
	  (λ ((X : T)) (C_2 ⚖ (@ 'Λ (λ ((X : T)) M) (C_3 ⚖ X)))))
   (havoc T_1 C_1 ...
	  (C_2 ⚖ (@ 'Λ (λ ((X : T)) M) (• T_0 C_3 C_0 ...))))]
  [(havoc (T_0 -> T_1) (C_0 -> C_1) ... M)
   (havoc T_1 (@ 'Λ M (• T_0 C_0 ...)))])

(define scv
  (union-reduction-relations sc (extend-reduction-relation v SCPCF)))

(define -->scv
  (union-reduction-relations (context-closure scv SCPCF E)
                             (extend-reduction-relation err-abort SCPCF)
                             (extend-reduction-relation con-abort SCPCF)))
