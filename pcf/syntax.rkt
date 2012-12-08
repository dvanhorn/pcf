#lang racket
(provide PCF)
(require redex/reduction-semantics)

(define-language PCF
  ;; Types
  (T ::= nat (T ... -> T))

  ;; Terms
  (M ::= X V (M M ...) (μ (X : T) V) (if0 M M M) (err T string) (err L T string))
  (V ::= N O (λ ([X : T] ...) M))

  (N ::= natural)
  (O ::= add1 sub1 * + quotient pos?)
  (X ::= variable-not-otherwise-mentioned)

  ;; Labels
  (L ::= any)

  ;; Evaluation contexts
  (E ::= hole (V ... E M ...) (if0 E M M))

  ;; Type environments
  (Γ ::= ((X T) ...)))
