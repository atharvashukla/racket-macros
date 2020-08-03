#lang racket




(make-set!-transformer
 (lambda (stx)
   (syntax-case stx (set!)
     [(set! id v)
      #'(set! id v)])))





(define a 3)
(set! a 1)