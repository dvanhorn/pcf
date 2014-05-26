#lang racket
(provide lab • ⚖ err μ -> if0 pp return)

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(require (for-syntax syntax/parse))
(define-syntax (• stx) (error 'kw))
(define-syntax (⚖ stx) (error 'kw))
(define-syntax (err stx) (error 'kw))
(define-syntax (μ stx) (error 'kw))
(define-syntax (if0 stx) (error 'kw))
  
(define-syntax (lab/l stx)
  (syntax-parse stx #:literals (• λ μ if0 err ⚖ ->)
    [(_ (• t) l) #'(list '• 't)]
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

(require unstable/lazy-require)
(lazy-require (redex/gui [default-pretty-printer]))

(define (loc stx)
  (and stx
       (string->symbol
	(format "~a:~a:~a"
		(syntax-source stx)
		(syntax-line stx)
		(syntax-column stx)))))

(define (scrub v)
  (match v
    [(list '@ l m ...)
     (map scrub m)]
    [(list 'err l t s)
     (list 'err (loc l) t s)]
    [(list 'blame s c0 c1 v)
     (list 'blame (loc s))]    
    [(list c + - '⚖ m)
     (list (scrub c) '⚖ (scrub m))]
    [(list m ...)
     (map scrub m)]
    [_ v]))

(define-struct (exn:fail:src exn:fail)
  (a-srcloc)
  #:property prop:exn:srclocs
  (lambda (a-struct)
    (match a-struct
      [(struct exn:fail:src
         (msg marks a-srcloc))
       (list a-srcloc)])))

(define (syntax->srcloc stx)
  (srcloc (syntax-source stx)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

(define (value? r)
          (match r
            [(list-rest 'blame l) #f]
            [(list 'err l t s) #f]
            [_ #t]))
        
(define (return res)
  (for ([r (in-list res)])
    (match r
      [(list 'blame l c0 c1 v) (raise-blame l c0 c1 v)]
      [(list 'err l t s) (raise-err l t s)]
      [_ (void)]))
  
  (apply values (map scrub (filter value? res))))
        
(define (syntax->srcstring stx)
  (if stx
      (format "~a:~a:~a:" 
              (syntax-source stx)
              (syntax-line stx)
              (syntax-column stx))
      ""))
              

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


(define pp
  (λ (v port width txt)
    (default-pretty-printer (scrub v) port width txt)))
