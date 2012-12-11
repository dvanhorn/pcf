#lang racket
(provide (rename-out [pcf-top #%top-interaction]
                     [pcf-module #%module-begin]))
(require (for-syntax syntax/parse))
(require redex/reduction-semantics
         pcf/source)
(define-syntax (pcf-top stx)
  (syntax-parse stx
    [(_ e)
     #'(#%top-interaction
         (apply values
                (apply-reduction-relation* -->v-source (term e))))]))

(define-syntax (pcf-module stx)
  (syntax-parse stx
    [(_ e ...)
     #'(#%module-begin
         (apply values
                (append (apply-reduction-relation* -->v-source (term e))
                        ...)))]))
