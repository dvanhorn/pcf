#lang racket
(provide subst ¬∈ transpose)
(require redex/reduction-semantics)

;; Subst
(define-language L (X variable) (T any))
(define-metafunction L
  subst : (X any) ... any -> any
  [(subst (X_1 any_1) (X_2 any_2) ... any_3)
   (subst-1 X_1 any_1 (subst (X_2 any_2) ... any_3))]
  [(subst any_3) any_3])

(define-metafunction L
  subst-1 : X any any -> any
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst-1 X_1 any_1 (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) any_2))
   (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) any_2)
   (side-condition (not (member (term X_1) (term (X_2 ...)))))]

  ;; MUST BE CALL BY VALUE, NOT DOING CAPTURE AVOIDING
  #;
  [(subst-1 X_1 any_1 (λ ([X_2 : T_2] ...) any_2))
   (λ ([X_new : T_2] ...)
     (subst-1 X_1 any_1 (subst-vars (X_2 X_new) ... any_2)))
   (where (X_new ...)
	  ,(variables-not-in (term (X_1 any_1 any_2))
			     (term (X_2 ...))))]
  
  [(subst-1 X_1 any_1 (λ ([X_2 : T_2] ...) any_2))
   (λ ([X_2 : T_2] ...)
     (subst-1 X_1 any_1 any_2))]

  ;; 3. replace X_1 with e_1
  [(subst-1 X_1 any_1 X_1) any_1]
  ;; 4. X_1 and X_2 are different, so don't replace
  [(subst-1 X_1 any_1 X_2) X_2]
  ;; the last cases cover all other expressions
  [(subst-1 X_1 any_1 (any_2 ...))
   ((subst-1 X_1 any_1 any_2) ...)]
  [(subst-1 X_1 any_1 any_2) any_2])

(define-metafunction L
  subst-vars : (X any) ... any -> any
  [(subst-vars (X_1 any_1) X_1) any_1]
  [(subst-vars (X_1 any_1) (any_2 ...))
   ((subst-vars (X_1 any_1) any_2) ...)]
  [(subst-vars (X_1 any_1) any_2) any_2]
  [(subst-vars (X_1 any_1) (X_2 any_2) ... any_3)
   (subst-vars (X_1 any_1) (subst-vars (X_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define-metafunction L
  ¬∈ : any any ... -> #t or #f
  [(¬∈ any any_1 ... any any_2 ...) #f]
  [(¬∈ any any_1 ...) #t])

(define (transpose m)
  (cond [(empty? (car m)) '()]
        [else
         (cons (map car m)
               (transpose (map cdr m)))]))

(module+ test
  (require rackunit)
  (check-equal? 
   (transpose '((a b c)
                (1 2 3)
                (5 6 7)))
   '((a 1 5)
     (b 2 6)
     (c 3 7))))
