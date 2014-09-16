#lang scpcf/heap

((• 1 ((nat -> nat) (nat -> nat) -> nat))
 (λ ([x : nat]) (/ 1 x))
 (λ ([y : nat]) (+ 1 y)))

((• 2 ((((nat -> nat) -> nat) -> nat) -> nat))
 (λ ([f : ((nat -> nat) -> nat)]) (f (λ ([x : nat]) (/ 1 x)))))

((• 3 ((nat -> nat) -> nat))
 (λ ([x : nat]) (/ 1 (- 10 x))))

; (No error)
((• 4 ((nat -> nat) -> nat))
 (λ ([x : nat]) (/ 1 (+ 10 x))))

((• 5 ((nat -> nat) -> nat))
 (μ (f : (nat -> nat))
    (λ ([n : nat])
      (if0 (zero? n) 1
           (+ (/ 2 (- 13 n)) (f (/ 1 (- 7 n))))))))
