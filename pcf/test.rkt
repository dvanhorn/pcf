#lang racket
(require pcf/syntax
         pcf/semantics
         redex/reduction-semantics)

(define-syntax-rule
  (test-->>v t1 t2)  
  (test-->> -->v (term t1) (term t2)))

(module+ test
  (test-->>v (@ a add1 2) 3)
  (test-->>v (if0 1 2 3) 3)
  (test-->>v (if0 0 1 2) 1)
  (test-->>v (@ a quotient 5 2) 2)
  (test-->>v (@ a quotient 5 0)
             (err a nat "Divide by zero"))
  (test-->>v (@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
             2)
  (test-->>v (@ a (μ (fact : (nat -> nat))
                     (λ ([n : nat])
                       (if0 n
                            1
                            (@ b * n (@ c fact (@ d sub1 n))))))
                5)
             120)
  (test-->>v (@ a add1 (@ b quotient 5 0))
             (err b nat "Divide by zero"))
  (test-->>v (@ a = 3 4) 1)
  (test-->>v (@ a = 3 3) 0)
  (test-->>v (@ a even? 0) 0)
  (test-->>v (@ a even? 3) 1)
  (test-->>v (@ a odd? 0) 1)
  (test-->>v (@ a odd? 3) 0))
