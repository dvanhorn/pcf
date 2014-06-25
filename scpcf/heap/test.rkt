#lang racket
(require scpcf/heap/syntax
         scpcf/heap/semantics
         cpcf/heap/test
         pcf/heap/test
         redex/reduction-semantics)

(define-metafunction/extension cfoldσ SCPCFΣ
  scfoldσ : (M Σ) -> M
  [(scfoldσ ((• T C ...) Σ)) (• T C ...)])

(make-tester test-->>scvσ -->scvσ scfoldσ)

(module+ test
  (test-->>scvσ (@ a add1 2) 3)
  (test-->>scvσ (if0 1 2 3) 3)
  (test-->>scvσ (if0 0 1 2) 1)
  (test-->>scvσ (@ a quotient 5 2) 2)
  (test-->>scvσ (@ a quotient 5 0)
                (err a nat "Divide by zero"))
  (test-->>scvσ (@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
                2)
  ;; Changes from previous semantics
  (test-->>scvσ (@ a (μ (fact : (nat -> nat))
                        (λ ([n : nat])
                          (if0 n
                               1
                               (@ b * n (@ c fact (@ d sub1 n))))))
                   5)
                (• nat))
  (test-->>scvσ (@ a add1 (@ b quotient 5 0))
                (err b nat "Divide by zero")))

(module+ test
  (test-->>scvσ (@ a add1 (• nat)) (• nat pos?))
  (test-->>scvσ (if0 (• nat) 2 3) 2 3)
  (test-->>scvσ (@ a (• (nat -> nat)) 7) (• nat))
  (test-->>scvσ (@ a quotient (• nat) 5) (• nat))
  (test-->>scvσ (@ a quotient 5 (• nat))
                (• nat)
                (err a nat "Divide by zero"))
  
  
  (test-->>scvσ (@ a (μ (fact : (nat -> nat))
                        (λ ([n : nat])
                          (if0 n
                               1
                               (@ b * n (@ c fact (@ d sub1 n))))))
                   (• nat))
                1
                (• nat))
  
  (test-->>scvσ (@ a add1 (@ b quotient 5 (• nat)))
                (• nat pos?)
                (err b nat "Divide by zero")))

(module+ test
  (test-->>scvσ (pos? + - pos? ⚖ 7) 7)
  (test-->>scvσ (pos? + - pos? ⚖ 0) (blame + pos? pos? 0))
  (test-->>scvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 7)
                7)
  (test-->>scvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 0)
                (blame - zero? pos? 0))
  (test-->>scvσ (@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) 0)) 7)
                (blame + zero? pos? 0))  
  (test-->>scvσ (@ a (λ ([f : (nat -> nat)])
                       ((λ ([x : nat]) (@ b f x)) + - zero? ⚖ 7))
                   pos?)
                7))

(module+ test
  (test-->>scvσ (pos? + - pos? ⚖ (• nat))
                (• nat pos?)
                (blame + pos? pos? (• nat zero?))))