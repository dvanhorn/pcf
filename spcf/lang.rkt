#lang racket
(require (for-syntax racket/base pcf/private/make-lang)
         (for-syntax (only-in spcf/redex typable/symbolic?))
         spcf/redex
         pcf/private/racket-pcf)
(provide #%top-interaction #%module-begin (all-from-out pcf/private/racket-pcf))
(define-syntax #%top-interaction (make-#%top-interaction #'-->sv typable/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->sv typable/symbolic?))
