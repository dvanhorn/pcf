#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../redex/scpcf.rkt" typable/contract/symbolic?))
         "../redex/scpcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'inj-scv #'-->scv typable/contract/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'inj-scv #'-->scv typable/contract/symbolic?))
