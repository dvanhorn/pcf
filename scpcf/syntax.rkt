#lang racket
(provide SCPCF)
(require redex/reduction-semantics
         cpcf/syntax)

(define-extended-language SCPCF CPCF  
  (V .... (• T C ...) (• L T C ...))
  ;(C ....)  
  (M .... Ω)
  (TC T C))
