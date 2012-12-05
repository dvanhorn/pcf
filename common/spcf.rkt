#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../redex/spcf.rkt" typable/symbolic?))
         "../redex/spcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'inj-sv #'-->sv typable/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'inj-sv #'-->sv typable/symbolic?))
