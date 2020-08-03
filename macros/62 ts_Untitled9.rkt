#lang racket

(define tmp 5)
(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [(_ a b)
       #'(let ([tmp a])
           (if tmp tmp b))]
      )))

(or #f 5)