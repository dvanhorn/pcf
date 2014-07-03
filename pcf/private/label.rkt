#lang racket
(provide lab • ⚖ err μ -> if0 verify)

(require (for-syntax scpcf/types
		     racket/match
		     redex/reduction-semantics))

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(require (for-syntax syntax/parse))

(define-syntax-rule (defkw id ...)
  (begin (define-syntax id (lambda (stx)
			     (raise-syntax-error #f "keyword used out of context" stx)))
	 ...))
(defkw • ⚖ err μ if0 verify)

(define-syntax (lab/l stx)
  (syntax-parse stx #:literals (• λ μ if0 err ⚖ -> verify)
    [(_ (verify c e) l)
     (match (judgment-holds (typeof/contract/symbolic () ,(syntax->datum #'e) T) T)
       [(list t)
	#`(list '@ 'HAVOC '(• (#,t -> nat))
		(list (lab/l c l) #'e 'HAVOC 'c '⚖ (lab/l e l)))])]
    [(_ (• t t0 ...) l) #'(list '• 't (lab/l t0 l) ...)]
    [(_ (λ ([x : t] ...) e) l)
     #'(list 'λ '([x : t] ...) (lab/l e l))]
    [(_ (μ (x : t) e) l)
     #'(list 'μ '(x : t) (lab/l e l))]
    [(_ (if0 e0 e1 e2) l)
     #'(list 'if0 (lab/l e0 l) (lab/l e1 l) (lab/l e2 l))]
    [(_ (~and (err t s) src) l)
     #`(list 'err #'src 't s)]
    [(_ (c (~and kw ⚖) e) l)
     #`(list (lab/l c l)
	     #'e
	     l
	     'c
	     '⚖ (lab/l e l))]
    [(_ (c0 ... -> c) l)
     #'(list (lab/l c0 l) ... '-> (lab/l c l))]
    [(_ (~and (e ...) src) l)
     #'(list '@ #'src (lab/l e l) ...)]
    [(_ e l) #''e]))

