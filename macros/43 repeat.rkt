#lang racket

(define-syntax (repeat stx)
  (define (n-copies n expr)
    (if (> n 0) (cons expr (n-copies (sub1 n) expr)) null))
  (syntax-case stx ()
    [(_ N E)
     (integer? (syntax-e #'N))
     #'(begin (n-copies (syntax-e #'N) #'E))]))


(define )