#lang racket
(provide return returnσ pp ppσ ppσ* color)
(require (only-in redex/gui term-node-parents term-node-labels))
(require redex/reduction-semantics scpcf/heap/semantics scpcf/syntax)
(require unstable/lazy-require)
(lazy-require (redex/gui [default-pretty-printer]))

(define (return res)
  (for ([r (in-list res)])
    (match r
      [(list 'blame l c0 c1 v)
       (unless (eq? l 'HAVOC)
	 (raise-blame l c0 c1 v))]
      [(list (list 'blame l c0 c1 v) 'with cs)
       (unless (eq? l 'HAVOC)
         (raise-blame l c0 c1 v cs))]
      [(list 'err l t s) (raise-err l t s)]
      [(list (list 'err l t s) 'with cs) (raise-err l t s cs)]
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

(define (raise-blame l c0 c1 v [cs (hash)])
  ((error-display-handler)
   (apply
    string-append
    (format "blaming: ~a~nbroke: ~a~nexpected: ~a~ngiven: ~a"
            (syntax->srcstring l)
            c0 c1 v)
    (cond
     [(hash-empty? cs) '("\n(Unable to come up with counterexample)")]
     [else
      (list*
       "\nPossible breaking context:"
       (for/list ([(a t) (in-hash cs)])
         (format "~n  ~a: ~a" (•/subscript a) (scrub t))))]))
   (exn:fail:src ""
		 (current-continuation-marks)
		 (and l (syntax->srcloc l)))))

(define (raise-err l t s [cs (hash)])
  ((error-display-handler)
   (apply
    string-append
    (format "err:~a ~a" (syntax->srcstring l) s)
    (cond
     [(hash-empty? cs) '("\n(Unable to come up with counterexample)")]
     [else
      (list*
       "\nPossible breaking context:"
       (for/list ([(a t) (in-hash cs)])
         (format "~n  ~a: ~a" (•/subscript a) (scrub t))))]))
   (exn:fail:src ""
                 (current-continuation-marks)
                 (and l (syntax->srcloc l)))))

(define (value? r)
	  (match r
	    ['blame #f]
	    [(list-rest 'blame l) #f]
            [(list (list-rest 'blame _) 'with _) #f]
	    [(list 'err l t s) #f]
            [(list (list* 'err _) 'with _) #f]
	    [_ #t]))

(define T? (redex-match? SCPCF T))
(define O? (redex-match? SCPCF O))
(define (scrub v)
  (match v
    [(list '@ l m ...)
     (map scrub m)]
    [(list '• TC ...) ; only keep simple contracts
     (list* '• (for/list ([tc TC] #:when (or (T? tc) (O? tc))) tc))]
    [(list 'err l t s)
     (list 'err #;(loc l) #;t s)]
    [(list 'blame 'HAVOC c0 c1 v)
     (list 'blame 'HAVOC)]
    [(list 'blame s c0 c1 v)
     'blame
     #;(list 'blame #;(loc s))]
    [(list c + - c0 '⚖ m)
     (list (scrub c) '⚖ (scrub m))]
    [(list m ...)
     (map scrub m)]
    [_ v]))

(define pp
  (λ (v port width txt)
    (default-pretty-printer (scrub v) port width txt)))

(define ppσ*
  (λ (v port width txt)
    (match v
      [(list m s)
       (default-pretty-printer (list (scrub m) (pp-heap s)) port width txt)])))

(define (pp-heap s)
  (for/vector ([(k v) (in-hash s)])
	      (list k '↦
		    (let ()
		      (define rs 	       
			(for/list ((x (in-set (second v)))) (scrub x)))
		      (if (empty? rs)
			  (scrub (first v))
			  (cons (scrub (first v)) rs))))))




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

;; Generate identifier such as "•₄₂"
(define (•/subscript n)
  (define (subscript n)
    (cond [(< n 0) (subscript (- n))]
          [(< n 10) (vector-ref #("₀" "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉") n)]
          [else (define-values (q r) (quotient/remainder n 10))
                (string-append (subscript q) (subscript r))]))
  (string->symbol (format "•~a" (subscript n))))
