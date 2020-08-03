#lang racket


(define-syntax or
  (lambda (stx)
    (define l (syntax->list stx))
    (define a (cadr l))
    (define b (caddr l))

    #;#'(let ([tmp a])
          (if tmp tmp b))

    (datum->syntax #f
                   (list (quote-syntax let)  (list (list (quote-syntax tmp) a))
                         (list (quote-syntax if) (quote-syntax tmp) (quote-syntax tmp) b))
                   )))

(or #f 5)

