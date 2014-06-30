#lang racket
(require spcf/heap/syntax
         spcf/heap/semantics
         pcf/heap/test
         redex/reduction-semantics)

(make-tester test-->>svσ -->svσ sfoldσ)

(module+ test
  (test-->>svσ (@ a add1 2) 3)
  (test-->>svσ (if0 1 2 3) 3)
  (test-->>svσ (if0 0 1 2) 1)
  (test-->>svσ (@ a quotient 5 2) 2)
  (test-->>svσ (@ a quotient 5 0)
               (err a nat "Divide by zero"))
  (test-->>svσ (@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
               2)
  ;; Changes from previous semantics
  (test-->>svσ (@ a (μ (fact : (nat -> nat))
                       (λ ([n : nat])
                         (if0 n
                              1
                              (@ b * n (@ c fact (@ d sub1 n))))))
                  5)
               (• nat))
  (test-->>svσ (@ a add1 (@ b quotient 5 0))
               (err b nat "Divide by zero"))
  (test-->>svσ (@ a = 3 4) 1)
  (test-->>svσ (@ a = 3 3) 0))

(module+ test
  (test-->>svσ (@ a add1 (• nat)) (• nat))
  (test-->>svσ (if0 (• nat) 2 3) 2 3)
  (test-->>svσ (@ a (• (nat -> nat)) 7) (• nat))
  (test-->>svσ (@ a quotient (• nat) 5) (• nat))
  (test-->>svσ (@ a quotient 5 (• nat)) 
               (• nat)
               (err a nat "Divide by zero"))
  

  (test-->>svσ (@ a (μ (fact : (nat -> nat))
                       (λ ([n : nat])
                         (if0 n
                              1
                              (@ b * n (@ c fact (@ d sub1 n))))))
                  (• nat))
               1
               (• nat))
  
  (test-->>svσ (@ a add1 (@ b quotient 5 (• nat)))
               (• nat)
               (err b nat "Divide by zero")))
