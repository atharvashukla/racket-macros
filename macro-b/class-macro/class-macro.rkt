#lang racket

(require rackunit)
(require (for-syntax syntax/parse))

;; -----------------------------------------------------------------------------

;; Class
;; - methods : (hashof symbol (object any ... -> any))
;; - field-positions : (hashof symbol integer)
(struct class-info (methods field-positions)) 


;; Object
;; - class  : Class
;; - fields : [Vectorof Any]
(struct object-info (class [fields #:mutable]))


;; creates out ObjectInfo representation of objects
(define (make-object c . args)
  (object-info c (list->vector args)))

;; gets an arg of o by referring to
;; the field's position in its class
(define (get-field o name)
  (vector-ref (object-info-fields o)
              (hash-ref (class-info-field-positions (object-info-class o)) name)))

;; sets the arg (name) of o
;; by referring to its position in class
(define (set-field! o name v)
  (vector-set! (object-info-fields o)
               (hash-ref (class-info-field-positions (object-info-class o)) name)
               v))

;; looks up a method in the class' hash by `name1`
(define (lookup-method o name)
  (hash-ref (class-info-methods (object-info-class o)) name))

;; -----------------------------------------------------------------------------

(begin-for-syntax
  (define class-args-hash (hash)))

;; -----------------------------------------------------------------------------


;; the point classs, concretely
(define point-class-concrete
  (class-info
   (hash 'get-x (lambda (this) (get-field this 'x))
         'get-y (lambda (this) (get-field this 'y))
         'set-x (lambda (this v) (set-field! this 'x v))
         'set-y (lambda (this v) (set-field! this 'y v)))
   (hash 'x 0 
         'y 1)))

(define a-pt (make-object point-class-concrete 0 5))

((lookup-method a-pt 'set-x) a-pt 10)
(check-equal? ((lookup-method a-pt 'get-x) a-pt) 10)
(check-equal? ((lookup-method a-pt 'get-y) a-pt) 5)

;; -----------------------------------------------------------------------------

(define-syntax send
  (lambda (stx)
    (syntax-parse stx
      [(_ obj (method-name:id arg ...))
       #'((lookup-method obj 'method-name) obj arg ...)])))

(send a-pt (set-x 10))
(check-equal? (send a-pt (get-x)) 10)
(check-equal? (send a-pt (get-y)) 5)

;; -----------------------------------------------------------------------------

(define-syntax class 
  (syntax-rules (define)
    [(_ [field-name ...]
        this-id
        (define (method-name arg ...) body ...)
        ...)
     (class-info
      (for/hash ([name '(method-name ...)]
                 [proc (list (lambda (this-id arg ...)
                               (define-field field-name this-id) ...
                               body ...)
                             ...)])
        (values name proc))
      (for/hash ([name '(field-name ...)]
                 [pos (in-naturals)])
        (values name pos)))]))

(define-syntax-rule (define-field field-name this-id)
  (define-syntax field-name
    (syntax-id-rules (set!)
      [(set! id v) (set-field! this-id 'field-name v)]
      [(id arg (... ...)) ((get-field this-id 'field-name) arg (... ...))]
      [id (get-field this-id 'field-name)])))


;; -----------------------------------------------------------------------------


(define point-class
  (class [x y]
    this
    (define (get-x) x)
    (define (get-y) y)
    (define (set-x v) (set! x v))
    (define (set-y v) (set! y v))))


(define a-pt-m (make-object point-class 0 5))
(send a-pt-m (set-x 10))
(check-equal? (send a-pt-m (get-x)) 10)
(check-equal? (send a-pt-m (get-y)) 5)

;; -----------------------------------------------------------------------------

(send a-pt-m (get-x))