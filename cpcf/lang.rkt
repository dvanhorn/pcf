#lang racket
(require (for-syntax racket/base pcf/private/make-lang)
         (for-syntax (only-in cpcf/redex typable/contract?))
         cpcf/redex
         pcf/private/racket-pcf)
(provide #%top-interaction #%module-begin (all-from-out pcf/private/racket-pcf))
(define-syntax #%top-interaction (make-#%top-interaction #'-->cv typable/contract?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->cv typable/contract?))
