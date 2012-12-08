#lang racket
(provide SPCF)
(require redex/reduction-semantics
         pcf/syntax)

(define-extended-language SPCF PCF
  ;; Values
  (V .... (• T)))