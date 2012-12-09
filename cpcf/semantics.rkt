#lang racket
(provide CPCF-source CPCF c cv con-abort -->cv)
(require redex/reduction-semantics pcf/semantics cpcf/syntax)

(define c
  (reduction-relation
   CPCF #:domain M
   (--> (M L_+ L_- ⚖ V) (if0 (@ 'Λ M V) V (blame L_+)) ?)
   (--> ((C ..._1 -> C_0) L_+ L_- ⚖ (λ ([X : T] ..._1) M))
        (λ ([X : T] ...)
          (C_0 L_+ L_- ⚖
               (@ 'Λ (λ ([X : T] ...) M)
                  (C L_- L_+ ⚖ X) ...)))
        η)))

(define cv
  (union-reduction-relations c (extend-reduction-relation v CPCF)))

(define con-abort
  (reduction-relation
   CPCF #:domain M
   (--> (in-hole E (blame L))
        (blame L)
        (where #t (not-mt? E))
        con-abort)))

(define-metafunction CPCF
  not-mt? : E -> #t or #f
  [(not-mt? hole) #f]
  [(not-mt? E) #t])

(define -->cv
  (union-reduction-relations (context-closure cv CPCF E)
                             (extend-reduction-relation err-abort CPCF)
                             con-abort))
