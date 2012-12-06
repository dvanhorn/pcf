#lang racket
(provide lab ⚖)

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(require (for-syntax syntax/parse))
(define-syntax (⚖ stx) (error 'kw))
(define-syntax (lab/l stx)
  (syntax-parse stx #:literals (⚖)
    [(_ (c (~and kw ⚖) e) l)
     #`(list (lab/l c l) 
             l 
             #'kw
             '⚖ (lab/l e l))]
    [(_ (e ...) l)
     #'(list (lab/l e l) ...)]
    [(_ e l) #''e]))
