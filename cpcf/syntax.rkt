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
  (M ::= .... (C L L C ⚖ M) B)
  (B ::= (blame L C C V)) ;; L broke C, expected C given V
  (E ::= .... (C L L C ⚖ E))
  (L ::= † 'variable any))
