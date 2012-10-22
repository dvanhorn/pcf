#lang racket 
(provide PCF v -->v δ δf typeof typable?)
(require redex/reduction-semantics "subst.rkt")

(define-language PCF
  ;; Types
  (T ::= nat (T ... -> T))
  
  ;; Terms
  (M ::= X V (M M ...) (if0 M M M) (err T string))
  (V ::= N O (λ ([X : T] ...) M))
     
  (N ::= natural)
  (O ::= add1 sub1 * + quotient)
  (X ::= variable-not-otherwise-mentioned)
  
  ;; Evaluation contexts
  (E ::= hole (M ... E V ...) (if0 E M M))
  
  ;; Type environments
  (Γ ::= ((X T) ...)))

(define v
  (reduction-relation
   PCF #:domain M
   (--> ((λ ([X : T] ..._1) M) V ..._1)
        (subst (X V) ... M)
        β)
   (--> (O V ...) M
        (judgment-holds (δ O (V ...) M))
        δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
        (side-condition (not (zero? (term N))))
        if0-f)))
   
(define -->v (context-closure v PCF E))

(define-judgment-form PCF
  #:mode (δ I I O)
  ;; Using this contract will make v non-reusable.
  ;#:contract (δ O (V ...) M)
  [(δ O (N_0 ...) M)
   (where M (δf O (N_0 ...)))])

(define-metafunction PCF
  δf : O (V ...) -> M
  [(δf add1 (N))           ,(add1 (term N))]
  [(δf sub1 (N))           ,(max 0 (sub1 (term N)))]
  [(δf * (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf quotient (N_0 0))    (err nat "Divide by zero")]
  [(δf quotient (N_0 N_1)) ,(quotient (term N_0) (term N_1))])

(define (typable? M)
  (cons? (judgment-holds (typeof () ,M T) T)))

(define-judgment-form PCF
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
  [(typeof Γ (λ ([X : T] ...) M) (T ... -> T_0))
   (typeof (extend Γ (X T) ...) M T_0)])

(define-metafunction PCF
  extend-one : Γ X T -> Γ
  [(extend-one (any_0 ... (X T_0) any_1 ...) X T)
   (any_0 ... (X T) any_1 ...)]
  [(extend-one (any ...) X T)
   ((X T) any ...)])

(define-metafunction PCF
  extend : Γ (X T) ... -> Γ
  [(extend Γ) Γ]
  [(extend Γ (X T) any ...)
   (extend (extend-one Γ X T) any ...)])
