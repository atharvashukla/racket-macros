#lang racket

(struct posn [x] #:extra-constructor-name make-posn)

(set! make-posn (lambda (x) 5))

(posn 3)