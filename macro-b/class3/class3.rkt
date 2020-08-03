#lang racket


(define (compute-indexes our-fields parent-fields)
  (for/list ([p parent-fields])
    (index-of our-fields p)))

(struct object [f])


(define point%
  (lambda (x y)
    (object
     (lambda (msg)
       (case msg
         [(get-x)
          (lambda ()
            x)])))))

(define 3point%
  (lambda (z y x)
    (object
     (lambda (msg)
       (case msg
         [(get-z)
          (lambda () z)]
         [(sum)
          (lambda () (+ x y z))]
         [else ((object-f (point% x y)) msg)])))))

(((object-f (3point% 1 2 3)) 'get-x))


;(define-class (point% x y) #f
;  [])

;(define-class (3point% z y x) point%
;  [])

