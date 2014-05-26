#lang racket
(provide CPCF-source CPCF c cv con-abort -->cv)
(require redex/reduction-semantics pcf/semantics cpcf/syntax)

(define c
  (reduction-relation
   CPCF #:domain M
   (--> (M L_+ L_- C ⚖ V) (if0 (@ 'Λ M V) V (blame L_+ C M V)) ?)
   (--> ((C_1 ..._1 -> C_0) L_+ L_- C ⚖ (λ ([X : T] ..._1) M))
        (λ ([X : T] ...)
          (C_0 L_+ L_- C ⚖
               (@ 'Λ (λ ([X : T] ...) M)
                  (C_1 L_- L_+ C ⚖ X) ...)))
        η)))

(define cv
  (union-reduction-relations c (extend-reduction-relation v CPCF)))

(define con-abort
  (reduction-relation
   CPCF #:domain M
   (--> (in-hole E B) B
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
