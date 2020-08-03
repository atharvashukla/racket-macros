#lang racket

;; declating a posn with an 
(struct posn (x y) #:extra-constructor-name make-posn)

(define old-posn posn)

;; hint: set!ing the extra constructor
(set! make-posn
      (lambda (x y)
        (when (not (number? x))
          (error 'posn "bad args m"))
        (when (not (number? y))
          (error 'posn "bad args n"))
        (old-posn x y)))

(posn "foo" 2)



