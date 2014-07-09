#lang racket
(require cpcf/syntax
         cpcf/semantics
         redex/reduction-semantics)

(module+ test
  (test-->> -->cv '(@ a add1 2) 3)
  (test-->> -->cv '(if0 1 2 3) 3)
  (test-->> -->cv '(if0 0 1 2) 1)
  (test-->> -->cv '(@ a quotient 5 2) 2)
  (test-->> -->cv '(@ a quotient 5 0)
                  '(err a nat "Divide by zero"))
  (test-->> -->cv '(@ a (λ ([x : nat]) (@ b quotient 5 x)) 2)
                  2)
  (test-->> -->cv '(@ a (μ (fact : (nat -> nat))
                          (λ ([n : nat])
                            (if0 n
                                 1
                                 (@ b * n (@ c fact (@ d sub1 n))))))
                     5)
                  120)
  (test-->> -->cv '(@ a add1 (@ b quotient 5 0))
                  '(err b nat "Divide by zero"))
  (test-->> -->cv '(@ a = 3 4) 1)
  (test-->> -->cv '(@ a = 3 3) 0))

(module+ test
  (test-->> -->cv '(pos? + - pos? ⚖ 7) 7)
  (test-->> -->cv '(pos? + - pos? ⚖ 0) '(blame + pos? pos? 0))
  (test-->> -->cv '(@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 7)
                  7)
  (test-->> -->cv '(@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) x)) 0)
                  '(blame - zero? pos? 0))
  (test-->> -->cv '(@ a ((pos? -> pos?) + - zero? ⚖ (λ ([x : nat]) 0)) 7)
                  '(blame + zero? pos? 0))  
  (test-->> -->cv '(@ a (λ ([f : (nat -> nat)])
                         ((λ ([x : nat]) (@ b f x)) + - zero? ⚖ 7))
                     pos?)
                  7)
  (test-->> -->cv '((if0 1 pos? zero?) + - zero? ⚖ 0) 0)
  (test-->> -->cv '(@ a (((if0 1 pos? zero?) -> (if0 0 pos? zero?)) + - zero? ⚖ (λ ([x : nat]) (@ b add1 x))) 0)
                  1))
