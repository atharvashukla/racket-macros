#lang racket

(require rackunit)

;; We just define simple macro wrappers for class and object
;; creation, most of the heavy lifting is done during runtime

(require (for-syntax syntax/parse))

;; --- DATA DEFINITIONS ---

;; ClassInfo
;; - parent : ClassInfo
;; - fields : [Listof Symbol]
;; - method-proc : [Hash (Symbol . Proc)]
(struct class-info [parent-class fields-names method-names/proc])

;; ObjectInfo
;; - class : ClassInfo (the class that the object implements)
;; - field->values : [Hash (Symbol . Any)]
(struct object-info [implemented-class object-field/value])


;; --- NEW and DOT functions ---

;; ClassInfo [Listof Any]  -> ObjectInfo
;; instantiates a new object that implements `class`
(define (new class . arguments)
  (let ([fields (class-info-fields-names class)])
    (if (= (length arguments) (length fields))
        (object-info class (make-hash (map cons fields arguments)))
        (apply raise-arity-error 'new (length fields) arguments))))

;; ObjectInfo Symbol [Listof Any] -> Any
;; traverses the class hierarchy and applies the object onto the correct method
(define (dot-f obj method-name . arguments)
  (define (dot-f-class class object method-name args)
    (cond [(not class) (error 'dot "object does not implement method")]
          [(hash-has-key? (class-info-method-names/proc class) method-name)
           (apply-object-on-method class obj method-name args)]
          [else (dot-f-class (class-info-parent-class class) object method-name args)]))
  (dot-f-class (object-info-implemented-class obj) object-info method-name arguments))

;; ClassInfo ObjectInfo Symbol [Listof Any] -> Any
;; ASSUMPTION: this class (not its parents) contains `method`
(define (apply-object-on-method class obj method-name arguments)
  (let (;; extracting the `method` from `method-proc` hash of `class`
        [method (hash-ref (class-info-method-names/proc class) method-name)]
        ;; field values that correspond to the class' fields
        [field-values (for/list ([field-value (class-info-fields-names class)])
                        (hash-ref (object-info-object-field/value obj) field-value))])
    (apply (apply method field-values) arguments)))

;; --- MACRO WRAPS ---

;; simple macro so that the method does not
;; have to be quoted. 
(define-syntax-rule (dot object (method args ...))
  (dot-f object 'method args ...))

;; class : Stx -> Stx
;; defines a class macro wrapping
;; simply creating a ClassInfo struct
(define-syntax class
  (syntax-parser
    ;; matching the structure of the class definition
    [(_ (field ...) superclass
        [method-name expr]
        ...)
     ;; creating the class-info struct
     #'(class-info
        superclass
        ;; putting the quote to turn fields into symbols
        (list 'field ...)
        ;; Hash between the method names and their proc
        (make-hash (list (cons 'method-name (λ (field ...) expr)) ...)))]))


;; --- EXAMPLES/TESTS ---

(define point1%
  (class (x) #f
    [get-x (λ () x)]
    [addn (λ (n m) (+ x (* n m)))]))

(define point2% 
  (class (x y) point1%
    [get-y (λ () y)]))


(define point3%
  (class (z y x) point2%
    [get-z (λ () z)]
    [sum (λ () (+ x y z))]))

(check-equal? (dot (new point3% 1 2 3) (sum)) 6)
(check-equal? (dot (new point1% 3) (addn 3 4)) 15)