#lang racket
(provide (all-defined-out))
(require redex/reduction-semantics)
(require pcf/private/subst)

(define-language PCF-source
  ;; Types
  (T ::= nat (T ... -> T))
  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) S) (if0 M M M) (err T string))
  ;; Values
  (V ::= N O (λ ([X : T] ...) M))
  ;; Simple terms
  (S ::= V X)
  ;; Naturals
  (N ::= natural)
  ;; Primitive operations
  (O ::= add1 sub1 * + quotient)
  ;; Variables
  (X ::= variable-not-otherwise-mentioned)
  ;; Type environments
  (Γ ::= ((X T) ...))
  ;; Evaluation contexts
  (E ::= hole (V ... E M ...) (if0 E M M)))

(define v-source
  (reduction-relation
   PCF-source #:domain M
   (--> ((λ ([X : T] ..._1) M) V ..._1)
   (subst (X V) ... M)
   β)
   (--> (μ (X : T) S)
        (subst (X (μ (X : T) S)) S)
        μ)
   (--> (O V ...) M
   (judgment-holds (δ O (V ...) M))
   δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
   (judgment-holds (nonzero? N))
   if0-f)))

(define-judgment-form PCF-source
  #:mode (nonzero? I)
  #:contract (nonzero? N)
  [(nonzero? N)
   (where (side-condition N (not (zero? (term N)))) N)])

(define err-abort
  (reduction-relation
   PCF-source #:domain M
   (--> (in-hole E (err T string))
   (err T string)
   (where #t (not-mt? E))
   err-abort)))

(define-metafunction PCF-source
  not-mt? : E -> #t or #f
  [(not-mt? hole) #f]
  [(not-mt? E) #t])

(define-judgment-form PCF-source
  #:mode (δ I I O)
  #:contract (δ O (V ...) M)
  [(δ O (N_0 ...) M)
   (where M (δf O (N_0 ...)))])

(define-metafunction PCF-source
  δf : O (V ...) -> M
  [(δf add1 (N))           ,(add1 (term N))]
  [(δf sub1 (N))           ,(max 0 (sub1 (term N)))]
  [(δf * (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf quotient (N_0 0))    (err nat "Divide by zero")]
  [(δf quotient (N_0 N_1)) ,(quotient (term N_0) (term N_1))])

(define -->v-source
  (union-reduction-relations 
   (context-closure v-source PCF-source E) 
   err-abort))

(define (typable? M)
  (cons? (judgment-holds (typeof () ,M T) T)))

(define-judgment-form PCF-source
  #:mode (typeof I I O)
  #:contract (typeof Γ M T)
  [(typeof Γ (err T string) T)]
  [(typeof Γ N nat)]
  [(typeof ((X_0 T_0) ... (X T) any_1 ...) X T)]
  [(typeof Γ add1 (nat -> nat))]
  [(typeof Γ sub1 (nat -> nat))]
  [(typeof Γ * (nat nat -> nat))]
  [(typeof Γ + (nat nat -> nat))]
  [(typeof Γ quotient (nat nat -> nat))]
  [(typeof Γ (if0 M_1 M_2 M_3) T)
   (typeof Γ M_1 nat)
   (typeof Γ M_2 T)
   (typeof Γ M_3 T)]
  [(typeof Γ (M M_0 ...) T)
   (typeof Γ M (T_0 ..._1 -> T))
   (typeof Γ M_0 T_0)
   ...]
  [(typeof Γ (μ (X : T) V) T)
   (typeof (extend Γ (X T)) V T)]
  [(typeof Γ (λ ([X : T] ...) M) (T ... -> T_0))
   (typeof (extend Γ (X T) ...) M T_0)])

(define-metafunction PCF-source
  extend-one : Γ X T -> Γ
  [(extend-one (any_0 ... (X T_0) any_1 ...) X T)
   (any_0 ... (X T) any_1 ...)]
  [(extend-one (any ...) X T)
   ((X T) any ...)])

(define-metafunction PCF-source
  extend : Γ (X T) ... -> Γ
  [(extend Γ) Γ]
  [(extend Γ (X T) any ...)
   (extend (extend-one Γ X T) any ...)])

(test-equal (judgment-holds (typeof () 7 nat)) #t)
(test-equal (judgment-holds (typeof ((f (nat -> nat))) (f 7) nat)) #t)
(test-equal (judgment-holds (typeof () (7 add1) nat)) #f)
(test-equal (judgment-holds (typeof () 7 T) T) '(nat))
(test-equal (judgment-holds (typeof ((f (nat -> nat))) (f 7) T) T) '(nat))
(test-equal (judgment-holds (typeof () (7 add1) T) T) '())


(test-equal 
 (apply-reduction-relation v-source
                           (term (add1 7)))
 '(8))

(test-equal
 (apply-reduction-relation v-source
                           (term ((λ ([f : (nat -> nat)]) (f 3)) sub1)))
 '((sub1 3)))

(test-equal
 (apply-reduction-relation* v-source
                            (term ((λ ([f : (nat -> nat)]) (f 3)) sub1))) 
 '(2))

(test-equal
 (apply-reduction-relation* -->v-source
                            (term (sub1 (add1 5)))) 
 '(5))

(test-equal
 (apply-reduction-relation* -->v-source
                            (term ((μ (fact : (nat -> nat))
                                      (λ ([n : nat])
                                        (if0 n
                                             1
                                             (* n (fact (sub1 n))))))
                                   5)))
 '(120))
