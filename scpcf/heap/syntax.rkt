#lang racket
(provide SCPCFΣ fin@ fin+)
(require redex/reduction-semantics
         scpcf/syntax)

(define-extended-language SCPCFΣ SCPCF
  (A ::= integer)
  (P ::= (& A))
  (M ::= .... P)
  (V ::= N O (λ ([X : T] ...) M) (P ... -> P) (• T C ...) (• L T C ...) Fin)
  (E ::= hole 
     (@ L P ... E M ...) 
     (if0 E M M)     
     (P ... E C ... -> C)
     (P ... -> E)           
     (E L L C ⚖ M)
     (P L L C ⚖ E))
  
  (F ::= .... (T -> nat) Fin)
  ;; Without, the ? rule in CPCFΣ breaks when lifted to SCPCΣ
  (Fin ::= (fin (T ... -> T) (A ... ↦ A) ...))
  
  (S ::= (TV C ...))
  (TV ::= T V)
  (Σ ::= (side-condition any_0 (hash? (term any_0))))) ; A -> S

;; Look up finite function or return `#f` if no match
(define-metafunction SCPCFΣ
  fin@ : Fin A ... -> A or #f
  [(fin@ (fin _ ... (A ... ↦ A_y) _ ...) A ...) A_y]
  [(fin@ _ _ ...) #f])

;; Extend finite function
(define-metafunction SCPCFΣ
  fin+ : Fin (A ... ↦ A) -> Fin
  [(fin+ (fin any ...) any_1) (fin any ... any_1)])
