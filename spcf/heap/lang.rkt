#lang racket
(require (for-syntax racket/base pcf/private/make-lang)
         (for-syntax (only-in spcf/redex typable/symbolic?))
         spcf/heap/semantics
         pcf/heap/semantics
	 pcf/private/racket-pcf
         pcf/private/label
         pcf/private/return)
(provide #%top-interaction #%module-begin
         (all-from-out pcf/private/label)
         (all-from-out pcf/private/racket-pcf))
(define-syntax #%top-interaction (make-#%top-interaction #'-->svσ typable/symbolic? #'injσ #'returnσ))
(define-syntax #%module-begin    (make-#%module-begin    #'-->svσ typable/symbolic? #'injσ #'returnσ #'pp #'color))
