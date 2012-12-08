#lang racket
(provide lab ⚖ err scrub pp)

(define-syntax (lab stx)
  (syntax-case stx ()
    [(_ e) #'(lab/l e #'e)]))

(require (for-syntax syntax/parse))
(define-syntax (⚖ stx) (error 'kw))
(define-syntax (err stx) (error 'kw))
(define-syntax (lab/l stx)
  (syntax-parse stx #:literals (⚖ err)
    [(_ (~and (err t s) src) l)
     #`(list 'err #'src 't s)]
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

(define (loc stx)
  (and stx
       (string->symbol
	(format "~a:~a:~a"
		(syntax-source stx)
		(syntax-line stx)
		(syntax-column stx)))))

(define (scrub v)
  (match v
    [(list 'err l t s)
     (list 'err (loc l) t s)]
    [(list 'blame s)
     (list 'blame (loc s))]
    [(list c + - '⚖ m)
     (list (scrub c) '⚖ (scrub m))]
    [(list m ...)
     (map scrub m)]
    [_ v]))

(define pp
  (λ (v port width txt)
    (default-pretty-printer (scrub v) port width txt)))
