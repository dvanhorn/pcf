#lang racket
(provide CPCFΣ)
(require redex/reduction-semantics
         cpcf/syntax)

(define-extended-language CPCFΣ CPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (V ::= N F (P ... -> P))
  (E ::= hole 
     (@ L P ... E M ...) 
     (if0 E M M)     
     (P ... E C ... -> C)
     (P ... -> E)           
     (E L L C ⚖ M)
     (P L L C ⚖ E))
  (Σ ::= (side-condition any_0 (hash? (term any_0)))))  