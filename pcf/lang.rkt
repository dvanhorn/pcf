#lang racket
(require (for-syntax racket/base pcf/private/make-lang)
         (for-syntax (only-in pcf/types typable?))
         pcf/redex
	 pcf/private/racket-pcf
         pcf/private/label)
(provide #%top-interaction #%module-begin
         (all-from-out pcf/private/label)
         (all-from-out pcf/private/racket-pcf))
(define-syntax #%top-interaction (make-#%top-interaction #'-->v typable?))
(define-syntax #%module-begin    (make-#%module-begin    #'-->v typable?))
