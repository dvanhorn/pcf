#lang racket
(provide PCF PCF-source recursive-function?)
(require redex/reduction-semantics)

(define-language PCF-source
  ;; Types
  (T ::= nat (T ... -> T))
  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) V) (if0 M M M) (err T string))
  ;; Values
  (V ::= N O (λ ([X : T] ...) M))
  ;; Naturals
  (N ::= natural)
  ;; Primitive operations
  (O ::= add1 sub1 * + quotient pos? zero? even? odd?)
  ;; Variables
  (X ::= variable-not-otherwise-mentioned)
  ;; Type environments
  (Γ ::= ((X T) ...)))

(define-extended-language PCF PCF-source
  ;; Labels
  (L ::= any)
  ;; Terms
  (M ::= .... (@ L M M ...) (err L T string))
  
  ;; Evaluation contexts
  (E ::= hole (@ L V ... E M ...) (if0 E M M))

  ;; Contexts
  (χ ::= hole
     (@ L M ... χ M ...)
     (if0 M ... χ M ...)
     (μ (X : T) χ)
     (λ ([X : T] ...) χ)))

(define recursive-function?
  (redex-match PCF
               (λ ([X_0 : T_0])
                 (in-hole χ (μ (X_1 : T_1)
                               (λ ([X_0 : T_0])
                                 (in-hole χ X_1)))))))
