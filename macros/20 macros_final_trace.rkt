#lang racket

(define-syntax define/trace
  (syntax-rules ()
    [(_ id expr)
     (begin
       (define tmp expr)
       (displayln tmp (current-output-port))
       (define-syntax id 
         (make-set!-transformer
          (lambda  (stx)
            (syntax-case stx (set!)
              [the-id (identifier? #'the-id) #'tmp]
              [(set! the-id e)
               #'(begin (set! tmp e)
                        (displayln tmp (current-output-port)))])))))]))

(define/trace d 10)

(set! d 21)

(set! d 12)