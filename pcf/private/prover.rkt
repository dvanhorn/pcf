#lang racket
(require scpcf/heap/syntax
         redex/reduction-semantics)
(provide ⊢)

(define Z3
  (getenv "Z3"))


(define-metafunction SCPCFΣ
  ! : A -> variable
  [(! A) ,(string->symbol (format "a~a" (term A)))])

(define-metafunction SCPCFΣ
  to-assert : A C -> any
  [(to-assert A pos?) (> (! A) 0)]
  [(to-assert A zero?) (= (! A) 0)]
  [(to-assert A (λ ([X : nat]) (@ _ O X (& A_1))))
   (O (! A) (! A_1))]
  [(to-assert A (λ ([X : nat]) (@ _ not (@ _ O X (& A_1)))))
   (not (O (! A) (! A_1)))]
  [(to-assert A (λ ([X : nat]) (@ _ O X N)))
   (O (! A) N)]
  [(to-assert A (λ ([X : nat]) (@ _ O X)))
   (to-assert A O)]
  [(to-assert A C) (= true true)])

; unify with above
(define-metafunction SCPCFΣ
  to-conclude : A C -> any
  [(to-conclude A pos?) (> (! A) 0)]
  [(to-conclude A zero?) (= (! A) 0)]
  [(to-conclude A (λ ([X : nat]) (@ _ O X (& A_1))))
   (O (! A) (! A_1))]
  [(to-conclude A (λ ([X : nat]) (@ _ not (@ _ O X (& A_1)))))
   (not (O (! A) (! A_1)))]
  [(to-conclude A (λ ([X : nat]) (@ _ O X N)))
   (O (! A) N)]
  [(to-conclude A (λ ([X : nat]) (@ _ O X)))
   (to-conclude A O)]
  [(to-conclude A C) #f])


;; BUG: to-assert, should default to trivial for assumptions
;; but not for conclusions.

(define-metafunction SCPCFΣ
  ⊢ : Σ A C -> ✓ or ✗ or ?
  [(⊢ Σ A C)
   ✓ 
   (where (TV C_0 ... C C_1 ...) (get Σ A))]  
  [(⊢ Σ A C)
   ✓ 
   (where (any ...) (to-conclude A C))
   (where unsat (z3 Σ (assert (not (any ...)))))]
  [(⊢ Σ A C) 
   ✗ 
   (where (any ...) (to-conclude A C))
   (where unsat (z3 Σ (assert (any ...))))]
  [(⊢ Σ A C) ?])


(define t
  (hash 1 `(• nat ,(set 'pos?))
        2 `(• nat ,(set '(λ ([x : nat]) (@ Λ = x (& 1)))))))


(define (declare a)
  `(declare-fun ,(term (! ,a)) () Int))

(define (assert a c)
  `(assert ,(term (to-conclude ,a ,c))))

(define (negate a c)
  `(assert (not ,(term (to-conclude ,a ,c)))))

(define (Σ->SMT Σ)  
  (for/fold ([ds (set)]
             [as (set)])
    (([a m] (in-hash Σ)))
    (match m
      [(list (? number? n) cs) 
       (values (set-add ds (declare a))                                                
               (set-union as
                          (set `(assert (= ,(term (! ,a)) ,n)))
                          (for/set ([c (in-set cs)])
                            (assert a c))))]
      [(list 'nat cs)
       (values (set-add ds (declare a))  
               (set-union as
                          (set `(assert (>= ,(term (! ,a)) 0)))
                          (for/set ([c (in-set cs)])
                            (assert a c))))]
      [_ (values ds as)])))



(define-metafunction SCPCFΣ
  z3 : Σ any ... -> sat or unsat or unknown
  [(z3 Σ any ...)
   ,(let ()
      (define p (make-temporary-file))
      (define-values (ds as) (Σ->SMT (term Σ)))
      (with-output-to-file p
        #:exists 'replace
        (λ ()      
          (for-each display
                    `[(set-option :produce-models true)
                      (set-logic QF_NIA)                  
                      ,@(for/list ([d (in-set ds)]) d)
                      ,@(for/list ([a (in-set as)]) a) 
                      ,@(term (any ...))
                      (check-sat)])))
      
      
      (begin0
        (match
            (with-output-to-string 
             (λ ()
               (if Z3
                   (system (format "~a -smt2 ~a" Z3 (path->string p)))
                   (error "No Z3 in environment"))))
          ["unsat\n" 'unsat]
          ["sat\n" 'sat]
          ["unknown\n" 'unknown])
        (delete-file p)))])
     
