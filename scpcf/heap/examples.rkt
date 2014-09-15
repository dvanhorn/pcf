#lang scpcf/heap

((• 1 ((nat -> nat) (nat -> nat) -> nat))
 (λ ([x : nat]) (/ 1 x))
 (λ ([y : nat]) (+ 1 y)))

((• 2 ((((nat -> nat) -> nat) -> nat) -> nat))
 (λ ([f : ((nat -> nat) -> nat)]) (f (λ ([x : nat]) (/ 1 x)))))
