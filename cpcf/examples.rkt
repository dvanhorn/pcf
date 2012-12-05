#lang cpcf

(add1 7)

((λ ([x : nat]) x) ⚖ 3)
((λ ([x : nat]) x) ⚖ 0)
(((λ ([x : nat]) x) -> (λ ([x : nat]) x))
 ⚖
 (λ ([x : nat]) x))

((((λ ([x : nat]) x) -> (λ ([x : nat]) x)) 
  ⚖
  (λ ([x : nat]) x))
 0)

((((λ ([x : nat]) x) -> (λ ([x : nat]) x)) 
  ⚖
  (λ ([x : nat]) x))
 7)

;; Safe x/10 function.
((((λ ([x : nat]) (if0 x 1 0)) -> (λ ([x : nat]) 0)) 
  ⚖
  (λ ([x : nat]) (quotient 10 x)))
 2)
 
                             

