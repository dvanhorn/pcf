#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../scpcf.rkt" typable/contract/symbolic?))
         "../scpcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'-->scv typable/contract/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->scv typable/contract/symbolic?))
