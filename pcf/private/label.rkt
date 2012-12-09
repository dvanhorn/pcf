#lang racket
(provide lab ⚖ err pp return)

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(require (for-syntax syntax/parse))
(define-syntax (⚖ stx) (error 'kw))
(define-syntax (err stx) (error 'kw))
(define-syntax (lab/l stx)
  (syntax-parse stx #:literals (⚖ err)
    [(_ (~and (err t s) src) l)
     #`(list 'err #'src 't s)]
    [(_ (c (~and kw ⚖) e) l)
     #`(list (lab/l c l)
             l
             #'kw
             '⚖ (lab/l e l))]
    [(_ (e ...) l)
     #'(list (lab/l e l) ...)]
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
    [(list 'err l t s)
     (list 'err (loc l) t s)]
    [(list 'blame s)
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
            [(list 'blame l) #f]
            [(list 'err l t s) #f]
            [_ #t]))
        
(define (return res)
  (for ([r (in-list res)])
    (match r
      [(list 'blame l) (raise-blame l)]
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
              

(define (raise-blame l)
  ((error-display-handler) (format "blame:~a" (syntax->srcstring l))
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
