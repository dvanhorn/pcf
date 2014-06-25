#lang racket
(provide SCPCFΣ)
(require redex/reduction-semantics
         scpcf/syntax)

(define-extended-language SCPCFΣ SCPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (E ::= hole (@ L P ... E M ...) (if0 E M M) (C L L C ⚖ E))
  (Σ ::= (side-condition any_0 (hash? (term any_0)))))