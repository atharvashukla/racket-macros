#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define (make-first-order n runtime)
    (lambda (stx)
      (syntax-parse stx
        [_:id (raise-syntax-error #f "cannot use first-order function as expression" stx)]
        [(_:id arg ...)
         (define cnt (length (syntax->list #'(arg ...))))
         (when (not (= cnt n))
           (raise-syntax-error #f "wrong argument count" stx))
         #`(#,runtime arg ...)]))))

(define-syntax define-first-order
  (syntax-parser
    [(_ (f args ...)
        body ...)
     #:with n (length (syntax->list #'(args ...)))
     #'(begin
         (define (tmp args ...)
           body ...)
         (define-syntax f (make-first-order 'n #'tmp)))]))

(define-first-order (f x y)
  (list x y))
#;
(when false
  (f 1 2))
#;
(when false
  f)

