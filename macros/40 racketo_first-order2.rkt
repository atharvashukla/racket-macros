#lang racket



(require (for-syntax syntax/parse racket/match))

(begin-for-syntax
  (struct first-order [n runtime]))

(define-syntax define-first-order
  (syntax-parser
    [(_ (f args ...)
        body ...)
     #:with n (length (syntax->list #'(args ...)))
     #'(begin
         (define (tmp args ...)
           body ...)
         (define-syntax f (first-order 'n #'tmp)))]))

(define-syntax apply-first-order
  (syntax-parser
    [(_ f:id args ...)
     (match-define (first-order n runtime) (syntax-local-value #'f))
     (when (not (= n (length (syntax->list #'(args ...)))))
       (raise-syntax-error #f "wrong argument count" this-syntax))
     #`(#,runtime args ...)]))

(define-first-order (f x y)
  (list x y))

(when false
  (apply-first-order f 1))

