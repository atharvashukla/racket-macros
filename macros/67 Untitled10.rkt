#lang racket


(define-syntax rec
  (syntax-rules ()
    [(_ x e) (letrec ([x e]) x)])) 

(map (rec sum (lambda (x) (if (= x 0)  0 (+ x (sum (- x 1))))))
     '(0 1 2 3 4 5))


(map (letrec ([sum (lambda (x)
                     (if (= x 0)
                         0
                         (+ x (sum (- x 1)))))])
       sum)
     '(0 1 2 3 4 5))