#lang racket
(provide make-#%top-interaction make-#%module-begin)
(require syntax/parse
         (for-template racket redex))

(define-syntax-class trace-opt
  [pattern (~datum stepper) #:attr sym 'stepper]
  [pattern (~datum traces)  #:attr sym 'traces])

(define ((make-#%top-interaction R) stx)
  (syntax-parse stx
    [(_ . e)
     #`(apply values (apply-reduction-relation* #,R (term e)))]))

(define ((make-#%module-begin R) stx)
  (syntax-parse stx
    [(_ (~optional trace:trace-opt #:defaults ([trace.sym #f]))
        e ...)
     (define trace (attribute trace.sym))
     #`(#%module-begin
        #,(if trace
              #'(reduction-steps-cutoff 100)
              #'(void))
        (initial-char-width 140)
        #,(case trace
            [(traces) 
             #`(begin (traces #,R (term e)) ...)]
            [(stepper)
             #`(begin (stepper #,R (term e)) ...)]
            [else
             #'(void)])
              
        (apply values (append (apply-reduction-relation* #,R (term e)) ...)))]))