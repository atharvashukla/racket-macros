#lang racket

(require (for-syntax syntax/parse))
(require rackunit)

(struct class-info
  [parent ; class-info
   fields ; (listof symbol?)
   methods ; (hash 'symbol procedure?)
   ])

(struct object
  [class ; class-info
      field-values ; (hash 'symbol any/c)
    ])

(define (make-class super fields methods)
  (class-info
   super
   fields
   (for/fold ([acc (hash)])
             ([pair methods])
     (hash-set acc (car pair) (cdr pair)))))

(define-syntax class
  (syntax-parser
    [(_ (field ...) superclass
        [method-name expr]
        ...)
     #'(make-class
        superclass
        (list 'field ...)
        (list (cons 'method-name
                    (lambda (field ...) expr)) ...))]))

(define (send-f obj method-name . args)
  (match-define (object cls vals) obj)

  (let loop ([cls cls])
    (when (not cls)
      (error 'send "object does not implement method"))
    
    (match-define (class-info parent fields methods) cls)
    (if (hash-has-key? methods method-name)
        (let ()
          (define method (hash-ref methods method-name))
          (define field-values
            (for/list ([f fields])
              (hash-ref vals f)))
          (apply (apply method field-values) args))
        (loop parent))))

(define-syntax-rule (send o (m args ...))
  (send-f o 'm args ...))

(define (new class . args)
  (match-define (class-info parent fields methods) class)

  (when (not (= (length args) (length fields)))
    (apply raise-arity-error 'new (length fields) args))
  
  (object class
          (for/fold ([acc (hash)])
                    ([field fields]
                     [arg args])
            (hash-set acc field arg))))


(define point1d
  (class (x) #f
    [get-x (lambda () x)]
    [distance-o (lambda () x)]
    [distance (lambda (x1) (abs (- x1 x)))]))

(define point2d
  (class (x y) point1d
    [get-y (lambda () y)]    
    [distance-origin (lambda () (sqrt (+ (sqr x) (sqr y))))]
    [distance (lambda (x1 y1) (sqrt (+ (sqr (- x1 x)) (sqr (- y1 y)))))]))

(define myposn2d (new point2d 4 3))

(check-equal? (send myposn2d (distance-origin)) 5)
(check-equal? (send myposn2d (distance 0 0)) 5)

(define point3d
  (class (x y z) point2d
    [get-z (lambda () z)]
    [distance-origin (lambda () (sqrt (+ (sqr x) (sqr y) (sqr z))))]
    [distance (lambda (x1 y1 z1) (sqrt (+ (sqr (- x1 x)) (sqr (- y1 y)) (sqr (- z1 z)))))]))

(define myposn3d (new point3d 4 3 5))




(check-equal? (send myposn3d (distance-origin)) (sqrt 50))
