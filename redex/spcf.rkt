#lang racket
(provide SPCF s sv inj-sv -->sv typeof/symbolic typable/symbolic?)
(require redex/reduction-semantics "pcf.rkt")

(define-extended-language SPCF PCF
  ;; Values
  (V .... (• T)))

(define s
  (reduction-relation
   SPCF #:domain M
   (--> ((• (T_0 ..._1 -> T)) V ..._1) (• T) β•)
   (--> ((• (T_0 ..._1 T T_1 ... -> T_o))
	     V_0 ..._1 V V_1 ...)
	(havoc T V)
	havoc)
   (--> (O V ...) M
	(judgment-holds (δ^ O (V ...) M))
	δ^)
   (--> (if0 (• nat) M_0 M_1) M_0 if•-t)
   (--> (if0 (• nat) M_0 M_1) M_1 if•-f)))

(define-metafunction SPCF
  inj-sv : M -> M
  [(inj-sv M) M])

(define sv
  (union-reduction-relations s (extend-reduction-relation v SPCF)))

(define-metafunction SPCF
  havoc : T M -> M
  [(havoc nat M) M]
  [(havoc (T_0 ... -> T_1) M)
   (havoc T_1 (M (• T_0) ...))])

(define-metafunction SPCF
  not-zero? : any -> #t or #f
  [(not-zero? 0) #f]
  [(not-zero? any) #t])

(define-metafunction SPCF
  not-div? : any -> #t or #f
  [(not-div? div) #f]
  [(not-div? any) #t])

(define -->sv (context-closure sv SPCF E))

(define-judgment-form SPCF
  #:mode (δ^ I I O)
  #:contract (δ^ O (V ...) M)
  [(δ^ quotient (any (• nat)) (• nat))]
  [(δ^ quotient (any (• nat)) (err nat "Divide by zero"))]
  [(δ^ quotient ((• nat) 0)   (err nat "Divide by zero"))]
  [(δ^ quotient ((• nat) N)   (• nat))
   (side-condition (not-zero? N))]
  [(δ^ O (any_0 ... (• nat) any_1 ...) (• nat))
   (side-condition (not-div? O))])

(define (typable/symbolic? M)
  (cons? (judgment-holds (typeof/symbolic () ,M T) T)))

(define-extended-judgment-form SPCF typeof
  #:mode (typeof/symbolic I I O)
  [(typeof/symbolic Γ (• T) T)])
