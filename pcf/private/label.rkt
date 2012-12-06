#lang racket
(provide lab ⚖ scrub pp)

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

(require unstable/lazy-require)
(lazy-require (redex/gui [default-pretty-printer]))

(define (scrub v)
  (match v
    [(list 'blame s)
     (list 'blame 
           (string->symbol
            (format "~a:~a:~a" 
                    (syntax-source s)
                    (syntax-line s)
                    (syntax-column s))))]
    [(list c + - '⚖ m)
     (list (scrub c) '⚖ (scrub m))]
    
    [(list m ...)
     (map scrub m)]
    [_ v]))

(define pp
  (λ (v port width txt)
    (default-pretty-printer (scrub v) port width txt)))