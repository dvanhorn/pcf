#lang racket
(provide SCPCF)
(require redex/reduction-semantics
         cpcf/syntax)

(define-extended-language SCPCF CPCF
  ;; Values
  (V .... (• T C ...))
  (C .... any?))
