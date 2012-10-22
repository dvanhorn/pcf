#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../cpcf.rkt" typable/contract?))
         "../cpcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'-->cv typable/contract?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->cv typable/contract?))
