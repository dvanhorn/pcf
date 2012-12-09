#lang racket
(provide make-pcf-tests)
(require redex/reduction-semantics pcf/redex
         (rename-in redex/reduction-semantics [term quote]))

(define (make-pcf-tests -->v typable?)
  (test-->> -->v '(@ #f add1 5) '6)
  (test-->> -->v '(@ #f sub1 5) '4)
  (test-->> -->v '(@ #f sub1 0) '0)
  (test-->> -->v '(@ #f * 3 4) '12)
  (test-->> -->v '(@ #f + 3 4) '7)
  (test-->> -->v '(@ #f pos? 0) '1)
  (test-->> -->v '(@ #f pos? 1) '0)
  (test-->> -->v '(@ #f quotient 1 0) '(err #f nat "Divide by zero"))
  (test-->> -->v '(@ #f add1 (@ #t quotient 1 0)) '(err #t nat "Divide by zero"))
  (test-->> -->v '(@ #f quotient 5 3) 1)
  (test-->> -->v '(@ #f quotient 6 3) 2)
  
  (test-->> -->v '(if0 0 1 2) '1)
  (test-->> -->v '(if0 1 1 2) '2)
  
  (test-->> -->v '(@ #f (λ ([x : nat]) x) 5) '5)
  (test-->> -->v '(@ #f (λ ([x : nat] [y : nat]) x) 5 7) '5)
  (test-->> -->v '(@ #t (err #f (nat -> nat) "l") (err #t nat "r"))
                 '(err #f (nat -> nat) "l"))
  (test-->> -->v '(@ #f (λ ([x : nat]) x) (err #f nat "r"))
                 '(err #f nat "r"))
  (test-->> -->v '(@ #t (@ #f (λ ([f : (nat -> nat)])
                      (λ ([x : nat])
                        (@ 'a f (@ 'b f x))))
                    add1)
                   3)
            '5)
  
  (test-->> -->v
            '(@ #f (μ (fact : (nat -> nat))
                 (λ ([n : nat])
                   (if0 n
                        1
                        (@ 'a * n (@ 'b fact (@ 'c sub1 n))))))
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