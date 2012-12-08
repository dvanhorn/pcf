#lang racket
(provide make-scpcf-tests)
(require redex/reduction-semantics scpcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-scpcf-tests -->scv typable/contract/symbolic?)
  (test-->> -->scv '(pos? + - ⚖ (• nat)) '(• nat pos?) '(blame +))
  (test-->> -->scv '((• (nat -> nat)) + - ⚖ 7) '7 '(blame +))
  (test-->> -->scv '(add1 (pos? + - ⚖ (• nat))) '(• nat) '(blame +))

  (test-equal (typable/contract/symbolic? '(pos? ⚖ (• nat))) #t)
  (test-equal (typable/contract/symbolic? '(pos? ⚖ (• (nat -> nat)))) #f)
  (test-equal (typable/contract/symbolic? '((• (nat -> nat)) ⚖ 7)) #t)
  (test-equal (typable/contract/symbolic? '((• (nat -> nat)) ⚖ (• nat))) #t))
