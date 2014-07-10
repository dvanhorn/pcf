#lang racket
(provide CPCF CPCF-source)
(require redex/reduction-semantics
         pcf/syntax)

(define-extended-language CPCF-source PCF
  ;; Terms
  (M ::= .... (C ... -> C) (C ⚖ M))
  (V ::= .... (V ... -> V))
  ;; Contracts
  (C ::= M)
  ;; Types
  (T ::= .... (Con T))) 

(define-extended-language CPCF CPCF-source
  (M ::= .... (C L L C ⚖ M) B)
  (B ::= (blame L C C M)) ;; L broke C, expected C given M
  (E ::= ....
     (V ... E C ... -> C)
     (V ... -> E)
     (E L L C ⚖ M)
     (V L L C ⚖ E))
  
  (L ::= † 'variable any))
