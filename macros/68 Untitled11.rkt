#lang racket


(define-syntax my-let
  (syntax-rules ()
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

;; (define-syntax keyword expr)
(define-syntax my-let*
  (syntax-rules ()
    [(_ () b1 b2 ...) (let () b1 b2 ...)]
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
     (let ([i1 e1])
       (let* ([i2 e2] ...) b1 b2 ...))]))


;; let-syntax and letrec-syntax
(let ([f (lambda (x) (+ x 1))])t
  (let-syntax ([f (syntax-rules ()
                       [(_ x) x])]
               [g (syntax-rules ()
                       [(_ x) (f x)])])
    (list (f 1) (g 1))))




