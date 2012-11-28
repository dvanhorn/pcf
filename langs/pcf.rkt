#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../redex/pcf.rkt" typable?))
         "../redex/pcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'-->v typable?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->v typable?))
