#lang racket

(require (for-syntax racket/base syntax/parse racket/match))
(require syntax/parse)
(require rackunit)

(define-struct class-info [fields f])
(define-struct object [f])

(define (compute-indexes our-fields parent-fields)
  ;; the position of each parent-fields's symbol in our-fields
  ;; ((listof symbol?) (listof symbol?) . -> . (listof number?))
  (for/list ([p parent-fields])
    (index-of our-fields p)))  

(define (select list indexes)
  ;; extracts each index-th symbol from list into a list
  ;; ((listof symbol?) (listof number?) . -> . (listof symbol?))
  (for/list ([index indexes])
    (list-ref list index)))



(define-syntax (define-class stx)
  (syntax-parse stx 
    [(_ (class-name class-fields ...)
        [method-name expr]
        ...)
     #'(define class-name 
         (class-info 
          '(class-fields ...)
          (lambda (class-fields ...)
            (make-object 
                (lambda (msg)
                  (case msg 
                    [(method-name) expr]
                    ...))))))]
    ;; here is the second case: where we're inheriting a parent class
    ;; todo: add the inherit key (instead of a simple parent after the class)
    [(_ (class-name class-fields ...) parent 
        [method-name expr]
        ...)
     #'(define class-name 
         (class-info 
          '(class-fields ...)
          (let* ([super parent]
                 [super-fields (class-info-fields super)]
                 [this-fields '(class-fields ...)]
                 ;; (compute-indexes '(z y x) '(x y)) => '(2 1)
                 [indexes-of-super-fields (compute-indexes this-fields super-fields)])
            (lambda (class-fields ...)
              (make-object
                  (lambda (msg)
                    (case msg
                      [(method-name) expr]
                      ...
                      [else (let ([list-of-ids-to-pass (select (list class-fields ...) indexes-of-super-fields)])
                              ;; unwrap the class to pass the ids, then unwrap the object to pass the msg
                              ((object-f (apply (class-info-f super) list-of-ids-to-pass)) msg))])))))))]))



(define-syntax (send stx)
  (syntax-parse stx
    [(_ (class-name args1 ...) (method args2 ...))
     #'(let* ([ci (class-info-f class-name)]
              [of (object-f (ci args1 ...))])
         ((of 'method)))]))


;; --- EXAMPLES and 


(define-class (point1% x)
  [get-x (λ () x)]
  [addn  (λ (n m) (+ x (* n m)))])

(define-class (point2% x y) point1%
  [get-y (lambda () y)]
  [distance-origin (lambda () (sqrt (+ (sqr x) (sqr y))))])

(define-class (point3% z y x) point2%
  [get-z (lambda () z)]
  [sum (lambda () (+ x y z))])


(check-equal? (send (point2% 3 4) (distance-origin)) 5)
(check-equal? (send (point3% 3 4 1) (get-x)) 1)

