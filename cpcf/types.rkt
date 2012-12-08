#lang racket
(provide typeof/contract typable/contract?)
(require scpcf/types
         cpcf/syntax
         redex/reduction-semantics)

(define (typable/contract? t)
  (and ((redex-match CPCF M) t)
       (typable/contract/symbolic? t)))

(define-judgment-form CPCF
  #:mode (typeof/contract I I O)
  #:contract (typeof/contract Γ M T)
  [(typeof/contract Γ M T)
   (typeof/contract/symbolic Γ M T)])
