#lang racket


(require racket/contract)

(define/contract (compute-indexes our-fields parent-fields)
  ;; the position of each parent-fields's symbol in our-fields
  ;; ((listof symbol?) (listof symbol?) . -> . (listof number?))
  (for/list ([p parent-fields])
    (index-of our-fields p)))  

(define (select list indexes)
  ;; extracts each index-th symbol from list into a list
  ;; ((listof symbol?) (listof number?) . -> . (listof symbol?))
  (for/list ([index indexes])
    (list-ref list index)))  

;; the class-info struct
(struct class-info [fields f])

;; the object struct
(struct object [f])


(define point2d
  (class-info
   (list 'x 'y)
   (lambda (x y)
     (object
      (lambda (msg)
        (case msg
          [(get-x) (lambda () x)]))))))

(define point3d
  (class-info
   (list 'z 'y 'x)
   (let ([parent point2d])

     (define indexes (compute-indexes
                      (list 'z 'y 'x)
                      (class-info-fields parent)))
     
     (lambda (z y x)
       (object
        (lambda (msg)
          (case msg
            [(get-z) (lambda () z)]
            [(sum) (lambda () (+ x y z))]
            [else ((object-f
                    (apply (class-info-f parent)
                           (select (list z y x) indexes)))
                   msg)])))))))

(((object-f ((class-info-f point3d) 1 2 3)) 'get-x))