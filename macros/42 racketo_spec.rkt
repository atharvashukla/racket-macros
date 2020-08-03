#lang racket

(define-class (point x y)
  [get-x (lambda () x)]
  [get-y (lambda () y)])

(define-class (3point z) #:inherit point
  [get-z (lambda () z)]
  [distance
   (lambda (p2)
     (sqrt
      (+
       ; note that we can refer to x and y without explicitly binding
       ; them, because we can find out about them from the superclass
       ; at compile time.
       (expt (- x (send p2 3point (get-x))) 2)
       (expt (- y (send p2 3point (get-y))) 2)
       (expt (- z (send p2 3point (get-z))) 2))))])

(define o1 (3point 1 1 1))
(define o2 (3point 2 2 2))

; send indicates the class 3point for static checking of method argument count
(send o1 3point (distance o2))

; this should be a compile-time error due to the wrong argument count:
(when #f
  (send o1 3point (distance)))