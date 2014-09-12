#lang racket
(provide SCPCFΣ)
(require redex/reduction-semantics
         scpcf/syntax)

(define-extended-language SCPCFΣ SCPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (V ::= N O (λ ([X : T] ...) M) (P ... -> P) (• T C ...))
  (E ::= hole 
     (@ L P ... E M ...) 
     (if0 E M M)     
     (P ... E C ... -> C)
     (P ... -> E)           
     (E L L C ⚖ M)
     (P L L C ⚖ E))
  
  (F ::= .... (T -> nat))
  ;; Without, the ? rule in CPCFΣ breaks when lifted to SCPCΣ
  
  (S ::= (TV C ...))
  (TV ::= T V)
  (Σ ::= (side-condition any_0 (hash? (term any_0))))) ; A -> S