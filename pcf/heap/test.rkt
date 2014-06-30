#lang racket
(require pcf/heap/syntax
         pcf/heap/semantics         
         redex/reduction-semantics)

(provide make-tester)

(define-syntax-rule
  (make-tester test-->>name -->name fold)  
  (define-syntax-rule
    (test-->>name T1 T2 (... ...))
    (test-->> -->name
              #:cycles-ok
              #:equiv (λ (t1 t2) 
                        (equal? (term (fold ,t1))
                                (term (fold ,t2))))
              (term (T1 ∅))
              (term (T2 ∅))
              (... ...))))

(make-tester test-->>vσ -->vσ foldσ)

(module+ test
  (test-->>vσ (@ a add1 2) 3)
  (test-->>vσ (if0 1 2 3) 3)
  (test-->>vσ (if0 0 1 2) 1)
  (test-->>vσ (@ a quotient 5 2) 2)
  (test-->>vσ (@ a quotient 5 0)
              (err a nat "Divide by zero"))
  (test-->>vσ (@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
              2)
  (test-->>vσ (@ a (μ (fact : (nat -> nat))
                      (λ ([n : nat])
                        (if0 n
                             1
                             (@ b * n (@ c fact (@ d sub1 n))))))
                 5)
              120)
  (test-->>vσ (@ a add1 (@ b quotient 5 0))
              (err b nat "Divide by zero"))
  (test-->>vσ (@ a = 3 4) 1)
  (test-->>vσ (@ a = 3 3) 0))
