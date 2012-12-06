#lang racket
(provide CPCF-source CPCF c cv -->cv inj-cv typeof/contract typable/contract?)
(provide lab/context lab-c/context typeof-contract) ; for documentation
(require redex/reduction-semantics pcf/redex)

(define-extended-language CPCF-source PCF
  ;; Terms
  (M ::= .... (C ⚖ M))
  ;; Contracts
  (C ::= V (C ... -> C)))

(define-extended-language CPCF CPCF-source
  (M ::= .... (C L L ⚖ M) (blame L))
  (E ::= .... (C L L ⚖ E))
  (L ::= † 'variable any))

(define-metafunction CPCF
  inj-cv : M -> M
  [(inj-cv M) (lab/context M †)])

(define-metafunction CPCF
  lab/context : M L -> M
  [(lab/context (C ⚖ M) L)
   ((lab-c/context C L) L L_0 ⚖ (lab/context M L_0))
   (where L_0 (quote ,(gensym)))]
  [(lab/context (M ...) L)
   ((lab/context M L) ...)]
  [(lab/context (if0 M ...) L)
   (if0 (lab/context M L) ...)]
  [(lab/context (λ ([X : T] ...) M) L)
   (λ ([X : T] ...) (lab/context M L))]
  [(lab/context M L) M])

(define-metafunction CPCF
  lab-c/context : C L -> C
  [(lab-c/context (C_0 ... -> C) L)
   ((lab-c/context C_0 L) ... -> (lab-c/context C L))]
  [(lab-c/context V L)
   (lab/context V L)])

(define c
  (reduction-relation
   CPCF #:domain M
   (--> (M L_+ L_- ⚖ V) (if0 (M V) V (blame L_+)) ?)
   (--> ((C ..._1 -> C_0) L_+ L_- ⚖ (λ ([X : T] ..._1) M))
        (λ ([X : T] ...)
	  (C_0 L_+ L_- ⚖
            ((λ ([X : T] ...) M)
             (C L_- L_+ ⚖ X) ...)))
	η)))

(define cv
  (union-reduction-relations c (extend-reduction-relation v CPCF)))

(define -->cv (context-closure cv CPCF E))

(define (typable/contract? M)
  (cons? (judgment-holds (typeof/contract () ,M T) T)))

(define-extended-judgment-form CPCF typeof
  #:mode (typeof/contract I I O)
  [(typeof/contract Γ (C ⚖ M) T)
   (typeof-contract Γ C T)
   (typeof/contract Γ M T)])

(define-judgment-form CPCF
  #:mode (typeof-contract I I O)
  #:contract (typeof-contract Γ C T)
  ;:interp C is a contract for values of type T
  [(typeof-contract Γ (C ... -> C_0) (T ... -> T_0))
   (typeof-contract Γ C_0 T_0)
   (typeof-contract Γ C T)
   ...]
  [(typeof-contract Γ M T)
   (typeof/contract Γ M (T -> nat))])
