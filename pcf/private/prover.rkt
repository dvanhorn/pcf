#lang racket
(require scpcf/heap/syntax
         redex/reduction-semantics)
(provide ⊢ z3/model)

(define Z3
  (getenv "Z3"))


(define-metafunction SCPCFΣ
  ! : A -> variable
  [(! A) ,(string->symbol (format "a~a" (term A)))])

(define-metafunction SCPCFΣ
  ¡ : variable -> A
  [(¡ variable)
   ,(match (symbol->string (term variable))
      [(regexp #rx"a(.+)" (list _ d)) (string->number d)])])

;; Z3 macro avoiding cluttering the formula with `and` in trivial cases
(define-metafunction SCPCFΣ
  AND : any ... -> any
  [(AND) true]
  [(AND any) any]
  [(AND any ...) (and any ...)])


#;(define-metafunction SCPCFΣ
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
  [(to-assert A sub1)
   (= (! A) 1)]
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
  [(to-conclude A (λ ([X : nat]) (@ _ = X (@ _ add1 (& A_1)))))
   (= (! A) (+ 1 (! A_1)))]
  [(to-conclude A (λ ([X : nat]) (@ _ = X (@ _ sub1 (& A_1)))))
   (= (! A) (- (! A_1) 1))]
  [(to-conclude A (λ ([X : nat]) (@ _ = X (@ _ ÷ (& A_1) ...))))
   (= (! A) (div (! A_1) ...))]
  [(to-conclude A (λ ([X : nat]) (@ _ = X (@ _ O (& A_1) ...))))
   (= (! A) (O (! A_1) ...))]
  [(to-conclude A sub1)
   (= (! A) 1)]
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
      [(list (list 'fin (list 'nat ... '-> 'nat) maps ...) _)
       ;; Assert that ((a₁ = b₁) ∧ ⋯ ∧ (aₙ = bₙ) ⇒ ((f a₁ ⋯ aₙ) = (f b₁ ⋯ bₙ)))
       (define assrt 
         (term
          (AND
           ,@(let go ([maps maps])
               (match maps
                 [(cons `(,x ... ↦ ,y) maps′)
                  (append (for/list ([m maps′])
                            (match-define `(,w ... ↦ ,v) m)
                            (term (=> (AND ,@(for/list ([xᵢ x] [wᵢ w])
                                               (term (= (! ,xᵢ) (! ,wᵢ)))))
                                      (= (! ,y) (! ,v)))))
                          (go maps′))]
                 ['() '()])))))
       (values ds (set-add as `(assert ,assrt)))]
      [_ (values ds as)])))


;; Ask Z3 whether (Σ ∧ any ...) is sat
(define-metafunction SCPCFΣ
  z3 : Σ any ... -> sat or unsat or unknown
  [(z3 Σ any ...)
   ,(let ()
      (define-values (ds as) (Σ->SMT (term Σ)))
      (match (query `((set-option :produce-models true)
                      (set-logic QF_NIA)                  
                      ,@(for/list ([d (in-set ds)]) d)
                      ,@(for/list ([a (in-set as)]) a) 
                      ,@(term (any ...))
                      (check-sat)))
        ["unsat\n" 'unsat]
        ["sat\n" 'sat]
        ["unknown\n" 'unknown]))])
     
;; Ask Z3 to build a model if (Σ ∧ any ...) is sat
(define-metafunction SCPCFΣ
  z3/model : Σ -> any #|#f or hash|#
  [(z3/model Σ)
   ,(let ()
      (define-values (ds as) (Σ->SMT (term Σ)))
      (match (query `((set-option :produce-models true)
                      (set-logic QF_NIA)                  
                      ,@(for/list ([d (in-set ds)]) d)
                      ,@(for/list ([a (in-set as)]) a) 
                      (check-sat)
                      (get-model)))
        [(regexp #rx"^sat(.*)" (list _ m))
         (with-input-from-string m
           (λ ()
             (match-define `(model ,lines ...) (read))
             (for/hash ([line lines])
               (match-define `(define-fun ,a () Int ,n) line)
               (values (term (¡ ,a)) n))))]
        [_ #f]))])

;; Call out to Z3 with given query as Sexp
(define (query sexp)
  ; Debug
  #;(begin (printf "About to query:~n")
         (for-each (curry printf "~a~n") sexp)
         (printf "~n"))
  (define p (make-temporary-file))
  (with-output-to-file p
    #:exists 'replace
    (λ () (for-each display sexp)))
  (begin0
      (with-output-to-string 
          (λ ()
            (if Z3
                (system (format "~a -smt2 ~a" Z3 (path->string p)))
                (error "No Z3 in environment"))))
    (delete-file p)))
