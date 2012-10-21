#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         "../cpcf.rkt")
(provide #%top-interaction #%module-begin)
(define-syntax #%top-interaction (make-#%top-interaction #'-->cv))
(define-syntax #%module-begin    (make-#%module-begin    #'-->cv))
