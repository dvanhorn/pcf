#lang racket
(provide CPCF CPCF-source)
(require redex/reduction-semantics
         pcf/syntax)

(define-extended-language CPCF-source PCF
  ;; Terms
  (M ::= .... (C ⚖ M))
  ;; Contracts
  (C ::= V (C ... -> C)))

(define-extended-language CPCF CPCF-source
  (M ::= .... (C L L ⚖ M) (blame L))
  (E ::= .... (C L L ⚖ E))
  (L ::= † 'variable any))
