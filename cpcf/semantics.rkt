#lang racket
(provide CPCF-source CPCF c cv con-abort -->cv)
(require redex/reduction-semantics pcf/semantics cpcf/syntax)

(define c
  (reduction-relation
   CPCF #:domain M
   (--> (F L_+ L_- C ⚖ V) (if0 (@ 'Λ F V) V (blame L_+ C F V)) ?)
   (--> ((V_1 ..._1 -> V_0) L_+ L_- C ⚖ (λ ([X : T] ..._1) M))
        (λ ([X : T] ...)
          (V_0 L_+ L_- C ⚖
               (@ 'Λ (λ ([X : T] ...) M)
                  (V_1 L_- L_+ C ⚖ X) ...)))
        η)))

(define cv
  (union-reduction-relations c (extend-reduction-relation v CPCF)))

(define con-abort
  (reduction-relation
   CPCF #:domain M
   (--> (in-hole E B) B
        (where #t (not-mt? E))
        con-abort)))

(define -->cv
  (union-reduction-relations (context-closure cv CPCF E)
                             (extend-reduction-relation err-abort CPCF)
                             con-abort))
