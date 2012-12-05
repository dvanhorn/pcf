#lang racket
(require (for-syntax racket/base "make-lang.rkt")
         (for-syntax (only-in "../redex/cpcf.rkt" typable/contract?))
         "../redex/cpcf.rkt"
         "racket-pcf.rkt")
(provide #%top-interaction #%module-begin (all-from-out "racket-pcf.rkt"))
(define-syntax #%top-interaction (make-#%top-interaction #'inj-cv #'-->cv typable/contract?))
(define-syntax #%module-begin    (make-#%module-begin    #'inj-cv #'-->cv typable/contract?))
