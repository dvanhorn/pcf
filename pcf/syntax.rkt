#lang racket
(provide PCF)
(require redex/reduction-semantics)

(define-language PCF-source
  ;; Types
  (T ::= nat (T ... -> T))
  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) V) (if0 M M M) (err T string))
  ;; Values
  (V ::= N O (λ ([X : T] ...) M))
  (N ::= natural)
  (O ::= add1 sub1 * + quotient pos?)
  (X ::= variable-not-otherwise-mentioned))
  
(define-extended-language PCF PCF-source
  ;; Labels
  (L ::= any)
  ;; Terms
  (M ::= .... (@ L M M ...) (err L T string))
  
  ;; Evaluation contexts
  (E ::= hole (@ L V ... E M ...) (if0 E M M))

  ;; Type environments
  (Γ ::= ((X T) ...)))
