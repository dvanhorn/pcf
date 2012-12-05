#lang spcf traces

(if0 (• nat) 1 2)

(add1 (if0 (• nat) 1 2))

(add1 (if0 (• nat) 1 (• nat)))

((λ ([x : nat]) 1) 2)

((λ ([f : (nat -> nat)])
   (f (f 5)))
 (λ ([n : nat])
   (quotient 625 n)))

((• ((nat -> nat) -> nat))
 (λ ([n : nat])
   (quotient 625 n)))

((λ ([f : (nat -> nat)])
   (f (f 5)))
 (• (nat -> nat)))

(quotient 625 (quotient 625 5))
