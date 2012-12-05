#lang racket
(provide make-#%top-interaction make-#%module-begin)
(require syntax/parse
         redex/reduction-semantics
         pcf/redex
         (for-template racket redex pcf/private/lexical))

(define-syntax-class trace-opt
  [pattern (~datum stepper) #:attr sym 'stepper]
  [pattern (~datum traces)  #:attr sym 'traces])

(define ((make-#%top-interaction INJ REL typable?) stx)
  (syntax-parse stx
    [(_ . e)
     (unless (typable? (syntax->datum #'e))
       (raise-syntax-error 'type-error "ill-typed expression" #'e))
     #`(apply values (apply-reduction-relation* #,REL (term (#,INJ e))))]))

(define ((make-#%module-begin INJ REL typable?) stx)
  (syntax-parse stx
    [(_ (~optional trace:trace-opt #:defaults ([trace.sym #f]))
        e ...)
     (for-each (λ (e)
                 (unless (typable? (syntax->datum e))
                   (raise-syntax-error 'type-error "ill-typed expression" e)))
               (syntax->list #'(e ...)))
     (define trace (attribute trace.sym))
     #`(#%module-begin
        (lexical e) ... ; lexical expansion for IDE integration
        #,(if trace
              #'(reduction-steps-cutoff 100)
              #'(void))
        (initial-char-width 140)
        #,(case trace
            [(traces)
             #`(begin (traces #,REL (term (#,INJ e))) ...)]
            [(stepper)
             #`(begin (stepper #,REL (term (#,INJ e))) ...)]
            [else
             #'(void)])

        (apply values (append (apply-reduction-relation* #,REL (term (#,INJ e))) ...)))]))