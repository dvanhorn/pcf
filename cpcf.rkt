#lang racket
(provide CPCF c cv -->cv typeof/contract typable/contract?)
(require redex/reduction-semantics "pcf.rkt")

(define-extended-language CPCF PCF
  ;; Terms
  (M ::= .... (C ⚖ M) blame)
  ;; Contracts
  (C ::= V (C ... -> C))

  (E ::= .... (C ⚖ E)))

(define c
  (reduction-relation
   CPCF #:domain M
   (--> (M ⚖ V) (if0 (M V) V blame) ?)
   (--> ((C ..._1 -> C_0) ⚖ (λ ([X : T] ..._1) M))
        (λ ([X : T] ...)
          (C_0 ⚖ ((λ ([X : T] ...) M) (C ⚖ X) ...)))
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
