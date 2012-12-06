#lang racket
(provide lab)

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(define-syntax (lab/l stx)
  (syntax-case stx (⚖)
    [(_ (c ⚖ e) l)
     #'(list (lab/l c l) l #'⚖ '⚖ (lab/l e l))]
    [(_ (e ...) l)
     #'(list (lab/l e l) ...)]
    [(_ e l) #''e]))
