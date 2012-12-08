#lang racket
(require (for-syntax racket/base pcf/private/make-lang)
         (for-syntax (only-in scpcf/redex typable/contract/symbolic?))
         scpcf/redex
         pcf/private/racket-pcf
         pcf/private/label)
(provide #%top-interaction #%module-begin ⚖ (all-from-out pcf/private/racket-pcf))
(define-syntax #%top-interaction (make-#%top-interaction #'-->scv typable/contract/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->scv typable/contract/symbolic?))