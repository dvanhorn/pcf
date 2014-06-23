#lang racket
(provide SPCFΣ)
(require spcf/syntax
         redex/reduction-semantics)

(define-extended-language SPCFΣ SPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (E ::= hole (@ L P ... E M ...) (if0 E M M))
  (Σ ::= (side-condition any_0 (hash? (term any_0)))))
