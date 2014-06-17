#lang racket
(provide (rename-out [pcf-top #%top-interaction]
                     [pcf-module #%module-begin]))
(require (for-syntax syntax/parse pcf/source))
(require redex/reduction-semantics
         pcf/source)

(define-for-syntax (type-error e)
  (raise-syntax-error 'type-error "ill-typed expression" #'e))

(define-syntax (pcf-top stx)
  (syntax-parse stx
    [(_ . e)
     (unless (typable? (syntax->datum #'e)) (type-error 'e))
    #'(#%top-interaction .
        (apply values
               (apply-reduction-relation* -->v-source t)))]))

(define-syntax (pcf-module stx)
  (syntax-parse stx
    [(_ e ...)
     #'(#%module-begin
         (apply values
                (append (let ((t (term e)))
                          (unless (typable? t) (type-error t))
                          (apply-reduction-relation* -->v-source t))
                        ...)))]))
