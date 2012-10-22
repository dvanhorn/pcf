#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../spcf.rkt" typable/symbolic?))
         "../spcf.rkt")
(provide #%top-interaction #%module-begin)
(define-syntax #%top-interaction (make-#%top-interaction #'-->sv typable/symbolic?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->sv typable/symbolic?))
