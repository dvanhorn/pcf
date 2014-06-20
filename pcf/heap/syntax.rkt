#lang racket
(provide PCFΣ)
(require pcf/syntax
         redex/reduction-semantics)

(define-extended-language PCFΣ PCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (E ::= hole (@ L P ... E M ...) (if0 E M M))
  (Σ ::= (side-condition any_0 (hash? (term any_0)))))