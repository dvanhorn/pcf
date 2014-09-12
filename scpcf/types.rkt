#lang racket
(provide (rename-out [typable? typable/contract/symbolic?]
                     [typeof typeof/contract/symbolic]))
(require scpcf/syntax
         redex/reduction-semantics)

(define (typable? M)
  (cons? (judgment-holds (typeof () ,M T) T)))

(define-judgment-form SCPCF
  #:mode (typeof-c I I O)
  #:contract (typeof-c Γ any T)
  [(typeof-c Γ M (Con T))
   (typeof Γ M (T -> nat))]  
  [(typeof-c Γ (M_1 ... -> M_n) (Con (T_1 ... -> T_n)))
   (typeof-c Γ M_1 (Con T_1))
   ...
   (typeof-c Γ M_n (Con T_n))])

(define-judgment-form SCPCF
  #:mode (typeof I I O)
  #:contract (typeof Γ any T)  
  [(typeof Γ (verify C M) nat)
   (typeof-c Γ C (Con T))
   (typeof Γ M T)] 
  [(typeof Γ (err T string) T)]
  [(typeof Γ N nat)]
  [(typeof ((X_0 T_0) ... (X T) any_1 ...) X T)]
  [(typeof Γ add1 (nat -> nat))]
  [(typeof Γ sub1 (nat -> nat))]
  [(typeof Γ zero? (nat -> nat))]
  [(typeof Γ even? (nat -> nat))]
  [(typeof Γ odd? (nat -> nat))]
  [(typeof Γ = (nat nat -> nat))]
  [(typeof Γ < (nat nat -> nat))]
  [(typeof Γ > (nat nat -> nat))]
  [(typeof Γ <= (nat nat -> nat))]
  [(typeof Γ >= (nat nat -> nat))]
  [(typeof Γ * (nat nat -> nat))]
  [(typeof Γ + (nat nat -> nat))]
  [(typeof Γ - (nat nat -> nat))]
  [(typeof Γ not (nat -> nat))]
  [(typeof Γ pos? (nat -> nat))]
  [(typeof Γ / (nat nat -> nat))]
  [(typeof Γ quotient (nat nat -> nat))]
  [(typeof Γ (if0 M_1 M_2 M_3) T)
   (typeof Γ M_1 nat)
   (typeof Γ M_2 T)
   (typeof Γ M_3 T)]
  [(typeof Γ (M M_0 ...) T)
   (typeof Γ M (T_0 ..._1 -> T))
   (typeof Γ M_0 T_0)
   ...]
  [(typeof Γ (μ (X : T) V) T)
   (typeof (extend Γ (X T)) V T)]  
  [(typeof Γ (λ ([X : T] ...) M) (T ... -> T_0))
   (typeof (extend Γ (X T) ...) M T_0)]
  
  ;; CPCF
  [(typeof Γ (C ⚖ M) T)
   (typeof-c Γ C (Con T))
   (typeof Γ M T)]
  [(typeof Γ (C_0 ... -> C) (Con (T_0 ... -> T)))
   (typeof-c Γ C_0 (Con T_0))
   ...
   (typeof-c Γ C (Con T))]
     
  ;; SCPCF
  [(typeof Γ (• T) T)]
  [(typeof Γ (• L T C ...) T)
   (typeof Γ (• T C ...) T)]
  [(typeof Γ (• T C C_1 ...) T)
   (typeof Γ (• T C_1 ...) T)
   (typeof-c Γ C (Con T))])


(define-metafunction SCPCF
  extend-one : Γ X T -> Γ
  [(extend-one (any_0 ... (X T_0) any_1 ...) X T)
   (any_0 ... (X T) any_1 ...)]
  [(extend-one (any ...) X T)
   ((X T) any ...)])

(define-metafunction SCPCF
  extend : Γ (X T) ... -> Γ
  [(extend Γ) Γ]
  [(extend Γ (X T) any ...)
   (extend (extend-one Γ X T) any ...)])



(module+ test
  (test-equal (judgment-holds (typeof () 5 T) T)
              '(nat))
  (test-equal (judgment-holds (typeof () pos? T) T)
              '((nat -> nat)))
  (test-equal (judgment-holds (typeof () (-> pos?) T) T)
              '((Con (-> nat))))
  (test-equal (judgment-holds (typeof () (-> (if0 0 pos? zero?)) T) T)
              '((Con (-> nat))))
  (test-equal (judgment-holds (typeof () (λ ([x : nat]) (-> (λ ([y : nat]) (= x y)))) T) T)
              '((nat -> (Con (-> nat)))))
  (test-equal (judgment-holds (typeof () (• nat pos?) T) T)
              '(nat)))
