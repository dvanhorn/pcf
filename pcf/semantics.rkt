#lang racket
(provide v err-abort -->v δ δf nonzero? not-mt?)
(require redex/reduction-semantics
         pcf/syntax
         pcf/private/subst)

(define v
  (reduction-relation
   PCF #:domain M
   (--> (@ L (λ ([X : T] ..._1) M) V ..._1)
	(subst (X V) ... M)
	β)
   (--> (μ (X : T) V)
        (subst (X (μ (X : T) V)) V)
        μ)
   (--> (@ L O V ...) M
	(judgment-holds (δ O L (V ...) M))
	δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
	(judgment-holds (nonzero? N))
	if0-f)))

(define-judgment-form PCF
  #:mode (nonzero? I)
  #:contract (nonzero? N)
  [(nonzero? N)
   (where (side-condition N (not (zero? (term N)))) N)])

(define err-abort
  (reduction-relation
   PCF #:domain M
   (--> (in-hole E (err L T string))
	(err L T string)
	(where #t (not-mt? E))
	err-abort)))

(define -->v
  (union-reduction-relations (context-closure v PCF E) err-abort))

(define-metafunction PCF
  not-mt? : any -> #t or #f
  [(not-mt? hole) #f]
  [(not-mt? any) #t])

(define-judgment-form PCF
  #:mode (δ I I I O)
  ;; Using this contract will make v non-reusable.
  ;#:contract (δ O (V ...) M)
  [(δ O L (N_0 ...) M)
   (where M (δf O L (N_0 ...)))])

(define-metafunction PCF
  δf : O L (V ...) -> M
  [(δf add1 L (N))           ,(add1 (term N))]
  [(δf sub1 L (N))           ,(max 0 (sub1 (term N)))]
  [(δf * L (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + L (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf - L (N_0 N_1))        ,(max 0 (- (term N_0) (term N_1)))]
  #;[(δf abs L (N))            ,(abs (term N))]
  [(δf pos? L (0))            1]
  [(δf pos? L (N))            0]
  [(δf zero? L (0))           0]
  [(δf zero? L (N))           1]
  [(δf even? L (N))           ,(if (even? (term N)) 0 1)]
  [(δf odd? L (N))            ,(if (odd? (term N)) 0 1)]
  [(δf = L (N_0 N_1))         ,(if (= (term N_0) (term N_1)) 0 1)]
  [(δf > L (N_0 N_1))         ,(if (> (term N_0) (term N_1)) 0 1)]
  [(δf < L (N_0 N_1))         ,(if (< (term N_0) (term N_1)) 0 1)]
  [(δf <= L (N_0 N_1))        ,(if (<= (term N_0) (term N_1)) 0 1)]
  [(δf >= L (N_0 N_1))        ,(if (>= (term N_0) (term N_1)) 0 1)]  
  [(δf not L (N))             ,(if (zero? (term N)) 1 0)]  
  [(δf ÷ L (N_0 0))    (err L nat "Divide by zero")]
  [(δf ÷ L (N_0 N_1)) ,(quotient (term N_0) (term N_1))])

