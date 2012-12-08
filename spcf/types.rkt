#lang racket
(provide typeof/symbolic typable/symbolic?)
(require scpcf/types
         spcf/syntax
         redex/reduction-semantics)

(define (typable/symbolic? t)
  (and ((redex-match SPCF M) t)
       (typable/contract/symbolic? t)))

(define-judgment-form SPCF
  #:mode (typeof/symbolic I I O)
  #:contract (typeof/symbolic Γ M T)
  [(typeof/symbolic Γ M T)
   (typeof/contract/symbolic Γ M T)])