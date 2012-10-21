#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         "../pcf.rkt")
(provide #%top-interaction #%module-begin)
(define-syntax #%top-interaction (make-#%top-interaction #'-->v))
(define-syntax #%module-begin    (make-#%module-begin    #'-->v))
