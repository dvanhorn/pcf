#lang pcf

(if0 7 1 2)

(add1 (if0 7 1 2))

((λ ([x : nat]) 1) 2)

((λ ([f : (nat -> nat)])
   (f (f 5)))
 (λ ([n : nat])
   (quotient 625 n)))

(quotient 625 (quotient 625 5))
