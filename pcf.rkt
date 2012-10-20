#lang racket 
(provide PCF v -->v)
(require redex/reduction-semantics "subst.rkt")

(define-language PCF
  ;; Types
  (T ::= nat (T ... -> T))
  
  ;; Terms
  (M ::= X V (M M ...) (if0 M M M) (err string))
  (V ::= N O (λ ([X : T] ...) M))
     
  (N ::= natural)
  (O ::= add1 sub1 * + quotient)
  (X ::= variable-not-otherwise-mentioned)
  
  ;; Evaluation contexts
  (E ::= hole (M ... E V ...) (if0 E M M)))

(define v
  (reduction-relation
   PCF
   (--> ((λ ([X : T] ..._1) M) V ..._1)
        (subst (X V) ... M)
        β)
   (--> (O V_0 ...) M
        (judgment-holds (δ O (V_0 ...) M))
        δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
        (side-condition (not (zero? (term N))))
        if0-f)))
   
(define -->v (context-closure v PCF E))

(define-judgment-form PCF
  #:mode (δ I I O)
  [(δ O (N_0 ...) M)
   (where M (δf O (N_0 ...)))])

(define-metafunction PCF
  [(δf add1 (N))           ,(add1 (term N))]
  [(δf sub1 (N))           ,(max 0 (sub1 (term N)))]
  [(δf * (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf quotient (N_0 0))    (err "Divide by zero")]
  [(δf quotient (N_0 N_1)) ,(quotient (term N_0) (term N_1))])
