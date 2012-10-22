#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../cpcf.rkt" typable/contract?))
         "../cpcf.rkt")
(provide #%top-interaction #%module-begin)
(define-syntax #%top-interaction (make-#%top-interaction #'-->cv typable/contract?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->cv typable/contract?))
