#lang racket
(provide make-pcf-tests)
(require redex/reduction-semantics pcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-pcf-tests -->v typable?)
  (test-->> -->v '(add1 5) '6)
  (test-->> -->v '(sub1 5) '4)
  (test-->> -->v '(sub1 0) '0)
  (test-->> -->v '(* 3 4) '12)
  (test-->> -->v '(+ 3 4) '7)
  (test-->> -->v '(pos? 0) '1)
  (test-->> -->v '(pos? 1) '0)
  (test-->> -->v '(quotient 1 0) '(err nat "Divide by zero"))
  (test-->> -->v '(quotient 5 3) 1)
  (test-->> -->v '(quotient 6 3) 2)
  
  (test-->> -->v '(if0 0 1 2) '1)
  (test-->> -->v '(if0 1 1 2) '2)
  
  (test-->> -->v '((λ ([x : nat]) x) 5) '5)
  (test-->> -->v '((λ ([x : nat] [y : nat]) x) 5 7) '5)
  
  (test-->> -->v '(((λ ([f : (nat -> nat)])
                      (λ ([x : nat])
                        (f (f x))))
                    add1)
                   3)
            '5)
  
  (test-->> -->v
            '((μ (fact : (nat -> nat))
                 (λ ([n : nat])
                   (if0 n
                        1
                        (* n (fact (sub1 n))))))
              5)
            '120)
  
  
  (test-equal (typable? '5) #t)
  (test-equal (typable? '(add1 5)) #t)
  (test-equal (typable? '(5 5)) #f)
  (test-equal (typable? '(if0 4 1 2)) #t)
  (test-equal (typable? '(if0 4 1 add1)) #f)
  (test-equal (typable? '(((λ ([f : (nat -> nat)])
                             (λ ([x : nat])
                               (f (f x))))
                           add1)
                          3))
              #t)
  (test-equal (typable? '(λ ([f : (nat -> nat)]) (f f))) #f)
  
  ); end make-pcf-tests