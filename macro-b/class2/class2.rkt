#lang racket


(define (compute-indexes our-fields parent-fields)
  (for/list ([p parent-fields])
    (index-of our-fields p)))

(define (select list indexes)
  (for/list ([index indexes])
    (list-ref list index)))

(struct class-info [fields f])
(struct object [f])

;(select (list 1 2 3) (list 2 1))

(define point%
  (class-info
   (list 'x 'y)
   (lambda (x y)
     (object
      (lambda (msg)
        (case
          [(get-x)
           (lambda ()
             x)]))))))

(define 3point%
  (class-info
   (list 'z 'y 'x)
   (let ([parent point%])

     (define indexes (compute-indexes
                      (list 'z 'y 'x)
                      (class-info-fields parent)))
     
     (lambda (z y x)
       (object
        (lambda (msg)
          (case
            [(get-z)
             (lambda () z)]
            [(sum)
             (lambda () (+ x y z))]
            [else ((object-f
                    (apply (class-info-f parent)
                           (select (list z y x) indexes)))
                   msg)])))))))

(((object-f ((class-info-f 3point%) 1 2 3)) 'get-x))