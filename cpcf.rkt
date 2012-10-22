#lang racket
(provide CPCF c cv -->cv)
(require redex/reduction-semantics "pcf.rkt")
 
(define-extended-language CPCF PCF
  ;; Terms
  (M .... (C ⚖ M) blame)
  ;; Contracts
  (C V (C ... -> C))
  (E .... (C ⚖ E)))

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
