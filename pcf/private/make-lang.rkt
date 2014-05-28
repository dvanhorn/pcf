#lang racket
(provide make-#%top-interaction make-#%module-begin)
(require syntax/parse
         redex/reduction-semantics
        
         pcf/redex
         (for-template racket 
                       redex/reduction-semantics 
                       redex/pict redex/gui
                       pcf/private/lexical
                       pcf/private/label))   
  

(define-syntax-class trace-opt
  [pattern (~datum stepper) #:attr sym 'stepper]
  [pattern (~datum traces)  #:attr sym 'traces])

(define ((make-#%top-interaction REL typable?) stx)
  (syntax-parse stx
    [(_ . e)
     (unless (typable? (syntax->datum #'e))
       (raise-syntax-error 'type-error "ill-typed expression" #'e))
     #`(return (apply-reduction-relation* #,REL (lab e)))]))

(define ((make-#%module-begin REL typable?) stx)
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
	(require unstable/lazy-require)
	(lazy-require [redex (traces stepper initial-char-width 
                                     term-node-children reduction-steps-cutoff
                                     default-pretty-printer)])        
 
        #,(if trace
              #'(reduction-steps-cutoff 100)
              #'(void))
        (initial-char-width 140)
        #,(case trace
            [(traces)
             #`(begin (traces #,REL (lab e) #:pp pp #:pred color) ...)]
            [(stepper)
             #`(begin (stepper #,REL (lab e) #:pp pp) ...)]
            [else
             #'(void)])
        
        (return
         (append (apply-reduction-relation* #,REL (lab e)) ...)))]))
