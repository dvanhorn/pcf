#lang racket
(provide make-cpcf-tests)
(require redex/reduction-semantics cpcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-cpcf-tests -->cv typable/contract?)
  (test-->> -->cv '(pos? + - ⚖ 1) '1)
  (test-->> -->cv '(pos? + - ⚖ 0) '(blame +))
  (test-->> -->cv '(add1 (pos? + - ⚖ 0)) '(blame +))
  ;; This doesn't trigger a bug caused by extension because by sheer
  ;; luck the monitor form looks like an applicaton in PCF.
  (test-->> -->cv '((λ ([x : nat]) (pos? + - ⚖ 1)) (err #f nat "e")) '(err #f nat "e"))

  (test-equal (typable/contract? '(pos? ⚖ 1)) #t)
  (test-equal (typable/contract? '(pos? ⚖ add1)) #f))
