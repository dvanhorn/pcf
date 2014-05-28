#lang racket
(provide (rename-out [typable? typable/contract/symbolic?]
                     [typeof typeof/contract/symbolic]))
(require scpcf/syntax
         redex/reduction-semantics)

(define (typable? M)
  (cons? (judgment-holds (typeof () ,M T) T)))

(define-judgment-form SCPCF
  #:mode (typeof I I O)
  #:contract (typeof Γ any T)
  [(typeof Γ (verify C M) nat)
   (typeof-contract Γ C T)
   (typeof Γ M T)]
  [(typeof Γ (err T string) T)]
  [(typeof Γ N nat)]
  [(typeof ((X_0 T_0) ... (X T) any_1 ...) X T)]
  [(typeof Γ add1 (nat -> nat))]
  [(typeof Γ sub1 (nat -> nat))]
  [(typeof Γ zero? (nat -> nat))]
  [(typeof Γ even? (nat -> nat))]
  [(typeof Γ odd? (nat -> nat))]
  [(typeof Γ * (nat nat -> nat))]
  [(typeof Γ + (nat nat -> nat))]
  [(typeof Γ pos? (nat -> nat))]
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
   (typeof-contract Γ C T)
   (typeof Γ M T)]
  
  ;; SCPCF
  [(typeof Γ (• T) T)]
  [(typeof Γ (• T C C_1 ...) T)
   (typeof Γ (• T C_1 ...) T)
   (typeof-contract Γ C T)])

(define-judgment-form SCPCF
  #:mode (typeof-contract I I O)
  #:contract (typeof-contract Γ C T)
  ;:interp C is a contract for values of type T
  [(typeof-contract Γ (C ... -> C_0) (T ... -> T_0))
   (typeof-contract Γ C_0 T_0)
   (typeof-contract Γ C T)
   ...]
  [(typeof-contract Γ M T)
   (typeof Γ M (T -> nat))])

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
