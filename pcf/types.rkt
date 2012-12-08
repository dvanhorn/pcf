#lang racket
(provide typeof typable?)
(require scpcf/types
         pcf/syntax
         redex/reduction-semantics)

(define (typable? t)
  (and ((redex-match PCF M) t)
       (typable/contract/symbolic? t)))

(define-judgment-form PCF
  #:mode (typeof I I O)
  #:contract (typeof Γ M T)
  [(typeof Γ M T)
   (typeof/contract/symbolic Γ M T)])
  
