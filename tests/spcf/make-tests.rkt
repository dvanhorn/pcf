#lang racket
(provide make-spcf-tests)
(require redex/reduction-semantics spcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-spcf-tests -->sv typable/symbolic?)
  (test-->> -->sv '(@ #f add1 (• nat)) '(• nat))
  (test-->> -->sv '(if0 (• nat) 1 2) 1 2)
  (test-->> -->sv '(@ #f (• (nat -> nat)) (• nat)) '(• nat))

  (test-equal (typable/symbolic? '(• nat)) #t)
  (test-equal (typable/symbolic? '(add1 (• nat))) #t)
  (test-equal (typable/symbolic? '((• ((nat -> nat) -> nat)) add1)) #t)
  (test-equal (typable/symbolic? '((• (nat -> nat)) add1)) #f)
  (test-equal (typable/symbolic? '((• nat) (• nat))) #f))
