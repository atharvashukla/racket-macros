#lang racket

(require (for-syntax syntax/parse
                     syntax/parse/define
                     racket/struct-info
                     racket/list
                     racket/string
                     syntax/transformer))


(begin-for-syntax
  (struct my-class-info [fields methods constructor]
    #:property
    prop:procedure
    (lambda (c-info stx)
      ((set!-transformer-procedure
        (make-variable-like-transformer (my-class-info-constructor c-info)))
       stx))))

(struct object [field-values method-values])

(define-syntax (class stx)
  (syntax-parse stx
    [(_ name [field ...])
     #'(begin
         (define constructor (lambda (field ...) (object (list field ...) '())))
         (define-syntax name (my-class-info (list 'field ...) '() #'constructor)))
     ]))

(class point [x y])
(point 1 2)


(map point (list 1 2) (list 3 4))

;(define-syntax point (my-class-info (list 'x 'y) '()))

(define-syntax (num-fields stx)
  (syntax-parse stx
    [(_ cls)
     (define info (syntax-local-value #'cls #f))
     (define l (length (my-class-info-fields info)))
     #`#,l]))

(num-fields point)
