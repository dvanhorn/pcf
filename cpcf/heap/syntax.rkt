#lang racket
(provide CPCFΣ)
(require redex/reduction-semantics
         cpcf/syntax)

(define-extended-language CPCFΣ CPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (E ::= hole (@ L P ... E M ...) (if0 E M M) (C L L C ⚖ E))
  (Σ ::= (side-condition any_0 (hash? (term any_0)))))
