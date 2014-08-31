#lang racket
(provide return returnσ pp ppσ color)
(require (only-in redex/gui term-node-parents term-node-labels))
(require redex/reduction-semantics scpcf/heap/semantics)
(require unstable/lazy-require)
(lazy-require (redex/gui [default-pretty-printer]))

(define (return res)
  (for ([r (in-list res)])
    (match r
      [(list 'blame l c0 c1 v)
       (unless (eq? l 'HAVOC)
	 (raise-blame l c0 c1 v))]
      [(list 'err l t s) (raise-err l t s)]
      [_ (void)]))

  (apply values (map scrub (filter value? res))))

(define (returnσ res)
  (return (map (λ (t) (term (scfoldσ ,t))) res)))


(define (color term node)
  (match term
    [(list 'err _ ...) #f]
    [(list 'blame 'HAVOC _ ...) "lightgray"]
    [(list 'blame _ ...) #f]
    [_ (if (havoc? node) "lightgray" #t)]))

(define (raise-blame l c0 c1 v)
  ((error-display-handler)
   (format "blaming: ~a~nbroke: ~a~nexpected: ~a~ngiven: ~a"
	   (syntax->srcstring l)
	   c0 c1 v)
   (exn:fail:src ""
		 (current-continuation-marks)
		 (and l (syntax->srcloc l)))))

(define (raise-err l t s)
  ((error-display-handler) (format "err:~a ~a" (syntax->srcstring l) s)
			   (exn:fail:src ""
					 (current-continuation-marks)
					 (and l (syntax->srcloc l)))))

(define (value? r)
	  (match r
	    [(list-rest 'blame l) #f]
	    [(list 'err l t s) #f]
	    [_ #t]))

(define (scrub v)
  (match v
    [(list '@ l m ...)
     (map scrub m)]
    [(list 'err l t s)
     (list 'err #;(loc l) #;t s)]
    [(list 'blame 'HAVOC c0 c1 v)
     (list 'blame 'HAVOC)]
    [(list 'blame s c0 c1 v)
     (list 'blame (loc s))]
    [(list c + - c0 '⚖ m)
     (list (scrub c) '⚖ (scrub m))]
    [(list m ...)
     (map scrub m)]
    [_ v]))

(define pp
  (λ (v port width txt)
    (default-pretty-printer (scrub v) port width txt)))

(define ppσ
  (λ (v port width txt)
    (default-pretty-printer (scrub (term (scfoldσ ,v))) port width txt)))

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
	  (syntax-line stx)
	  (syntax-column stx)
	  (syntax-position stx)
	  (syntax-span stx)))

(define (syntax->srcstring stx)
  (if stx
      (format "~a:~a:~a:"
	      (syntax-source stx)
	      (syntax-line stx)
	      (syntax-column stx))
      ""))

(define-struct (exn:fail:src exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:src
	 (msg marks a-srcloc))
       (list a-srcloc)])))

(define (loc stx)
  (and stx
       (string->symbol
	(format "~a:~a:~a"
		(syntax-source stx)
		(syntax-line stx)
		(syntax-column stx)))))

(define (havoc? node)
  (member "havoc" (ancestor-rules (set) (set node))))

(define (ancestor-rules seen to-visit)
  (cond [(set-empty? to-visit)
	 (apply append (for/list ([s seen]) (term-node-labels s)))]
	[else
	 (define a (set-first to-visit))
	 (define to-visit* (set-union to-visit (apply set (term-node-parents a))))
	 (ancestor-rules (set-add seen a) (set-subtract to-visit* (set-add seen a)))]))

