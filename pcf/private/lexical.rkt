#lang racket
(provide lexical)
(require pcf/private/label)

(define-syntax (lexical stx)
  (syntax-case stx ()
    [(_ e) #'(void (λ () (lx e)))]))

(require (for-syntax syntax/stx))
(define-syntax (lx stx)
  (syntax-case stx (λ • : err μ ⚖)
     [(_ (λ ([x : _] ...) e))
      (nice stx #'(λ (x ...) (lx e)))]
     [(_ (μ (x : _) v))
      (nice stx #'(letrec ((x (lx v))) x))]
     [(_ (if0 e0 e1 e2))
      (nice stx #'(if (lx e0)
                      (lx e1)
                      (lx e2)))]
     [(_ (err _ string)) #'string]
     [(_ (• t ...)) #''(• t ...)]
     [(_ (c ⚖ e))
      (let ([kw (stx-car (stx-cdr (stx-car (stx-cdr stx))))])
        (syntax-track-origin #'(begin (lc c) #;(void ⚖) (lx e)) kw
                             (syntax-local-introduce kw)))]
     [(_ (e ...))
      #'((lx e) ...)]
     [(_ e) #'e]))

(define-for-syntax (nice ctx stx)
  (syntax-track-origin  
   stx
   (stx-car (stx-cdr ctx))
   (syntax-local-introduce (stx-car (stx-car (stx-cdr ctx))))))

(define-syntax (lc stx)
  (syntax-case stx (->)
    [(_ (c0 ... -> c1))
     #'(begin (lc c0) ... (lc c1))]
    [(_ e) #'(lx e)]))