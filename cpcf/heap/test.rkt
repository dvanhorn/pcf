#lang racket
(provide cfoldσ)
(require cpcf/heap/syntax
         cpcf/heap/semantics
         pcf/heap/test
         redex/reduction-semantics)

(make-tester test-->>cvσ -->cvσ cfoldσ)

(module+ test
  (test-->>cvσ (@ a add1 2) 3)
  (test-->>cvσ (if0 1 2 3) 3)
  (test-->>cvσ (if0 0 1 2) 1)
  (test-->>cvσ (@ a quotient 5 2) 2)
  (test-->>cvσ (@ a quotient 5 0)
               (err a nat "Divide by zero"))
  (test-->>cvσ (@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
               2)
  (test-->>cvσ (@ a (μ (fact : (nat -> nat))
                       (λ ([n : nat])
                         (if0 n
                              1
                              (@ b * n (@ c fact (@ d sub1 n))))))
                  5)
               120)
  (test-->>cvσ (@ a add1 (@ b quotient 5 0))
               (err b nat "Divide by zero"))
  (test-->>cvσ (@ a = 3 4) 1)
  (test-->>cvσ (@ a = 3 3) 0))

(module+ test
  (test-->>cvσ (pos? + - pos? ⚖ 7) 7)
  (test-->>cvσ (pos? + - pos? ⚖ 0) (blame + pos? pos? 0))
  (test-->>cvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 7)
               7)
  (test-->>cvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 0)
               (blame - zero? pos? 0))
  (test-->>cvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) 0)) 7)
               (blame + zero? pos? 0))  
  (test-->>cvσ (@ a (λ ([f : (nat -> nat)])
                      ((λ ([x : nat]) (@ b f x)) + - zero? ⚖ 7))
                  pos?)
               7))
                  
               

