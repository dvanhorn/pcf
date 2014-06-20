#lang pcf/heap
((μ (fact : (nat -> nat))
    (λ ([n : nat])
      (if0 n
           1
           (* n (fact (sub1 n))))))
 5)
