#lang racket
(provide SPCF s -->s)
(require redex/reduction-semantics "subst.rkt" "pcf.rkt")

(define-extended-language SPCF PCF
  ;; Values
  (V .... (• T)))
  
(define s
  (reduction-relation
   SPCF
   (--> ((λ ([X : T] ..._1) M) V ..._1)
        (subst (X V) ... M)
        β)
   (--> ((• (T ..._1 -> T_0)) V ..._1)
        (• T_0)
        β•)
   (--> ((• (T_0 ..._1 T T_1 ... -> T_o)) V_0 ..._1 V V_1 ...)
        (havoc T V)
        havoc)
   (--> (O V_0 ...) M
        (judgment-holds (δ^ O (V_0 ...) M))
        δ)
   (--> (if0 0 M_0 M_1) M_0 if0-t)
   (--> (if0 N M_0 M_1) M_1
        (side-condition (not (zero? (term N))))
        if0-f)
   (--> (if0 (• nat) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat) M_0 M_1) M_1 if•-f)))
   
(define-metafunction SPCF
  [(havoc nat M) M]
  [(havoc (T_0 ... -> T_1) M)
   (havoc T_1 (M (• T_0) ...))])

(define-metafunction SPCF
  [(not-zero? 0) #f]
  [(not-zero? any) #t])

(define-metafunction SPCF
  [(not-div? div) #f]
  [(not-div? any) #t])

(define -->s (context-closure s SPCF E))

(define-judgment-form SPCF
  #:mode (δ^ I I O)
  [(δ^ quotient (any (• nat)) (• nat))]  
  [(δ^ quotient (any (• nat)) (err "Divide by zero"))]  
  [(δ^ quotient ((• nat) 0)   (err "Divide by zero"))]  
  [(δ^ quotient ((• nat) N)   (• nat))
   (side-condition (not-zero? N))]
  [(δ^ O (any_0 ... (• nat) any_1 ...) (• nat))
   (side-condition (not-div? O))]
  [(δ^ O (N_0 ...) M)
   (where M (δf O (N_0 ...)))])

(define-metafunction PCF
  [(δf add1 (N))           ,(add1 (term N))]
  [(δf sub1 (N))           ,(max 0 (sub1 (term N)))]
  [(δf * (N_0 N_1))        ,(* (term N_0) (term N_1))]
  [(δf + (N_0 N_1))        ,(+ (term N_0) (term N_1))]
  [(δf quotient (N_0 0))    (err "Divide by zero")]
  [(δf quotient (N_0 N_1)) ,(quotient (term N_0) (term N_1))])