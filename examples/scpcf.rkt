#lang pcf/contracts/symbolic traces
(add1 (• nat pos?))

((• ((nat -> nat) -> nat))
 (λ ([n : nat])
   (quotient 625 n)))

((• ((nat -> nat) -> nat))
 ((pos? -> (λ ([x : nat]) 0))
  ⚖
  (λ ([n : nat])
    (quotient 625 n))))