#lang racket


(define c (class (x y)
  (distance (p2)
    ...)))

(c 1 2)

(define c2 (class c ()
             (define (get-x)
               x)))

(send (c2 1 2) (get-x))


(send (c 1 2) (get-x))