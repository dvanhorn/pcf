#lang racket
(provide make-cpcf-tests)
(require redex/reduction-semantics cpcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-cpcf-tests -->cv typable/contract?)
  (test-->> -->cv '(pos? + - ⚖ 1) '1)
  (test-->> -->cv '(pos? + - ⚖ 0) '(blame +))
  (test-->> -->cv '(@ #f add1 (pos? + - ⚖ 0)) '(blame +))
  ;; This is a bug caused by extension because the monitor form
  ;; is not an expression in PCF and the err-abort rule relies
  ;; on a PCF helper function that is not extended.
  (test-->> -->cv '((λ ([x : nat]) (pos? + - ⚖ 1)) (err #f nat "e")) '(err #f nat "e"))

  (test-equal (typable/contract? '(pos? ⚖ 1)) #t)
  (test-equal (typable/contract? '(pos? ⚖ add1)) #f))
