#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         "../spcf.rkt")
(provide #%top-interaction #%module-begin)
(define-syntax #%top-interaction (make-#%top-interaction #'-->sv))
(define-syntax #%module-begin    (make-#%module-begin    #'-->sv))
