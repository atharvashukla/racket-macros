#lang racket

;; the require for syntax/parse
(require (for-syntax syntax/parse))

;; --------------------------------------------------

;; ClassInfo is a struct
;;
;; it represents a Class in  our object system.
;;
;; parent is (U #f class-info)
;; - #f stands for no super class
;; - class-info (if specified) is the super class
;; 
;; fields is a [Listof Symbols] which
;; are the fields of ClassInfo
;;
;; methods is a (hash 'symbol procedure?)
;; a hash of the method name and their body (procedure)

(struct class-info
  [parent fields methods])

;; An Object is a struct
;;
;; it represents an object in out object system
;;
;; class is  a ClassInfo
;; the class that the object belongs to
;; 
;; field-values is a (hash 'symbol any/c)
;; a hash of the field with the value that it contains

(struct object
  [class field-values])

;; --------------------------------------------------

;; accepts the initial values to construct a class
;; using the struct we have defined. generates
;; the hasf from the methods 
(define (make-class super fields methods)
  (let  ([generated-hash
          (for/fold ([acc (hash)])
                    ([pair methods])
            (hash-set acc (car pair) (cdr pair)))])
    (class-info super fields)))


;; the simple macro that adds this syntactic
;; "wrap" around the class and uses our
;; make-class method to do all the heavy-lifting
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

;; calls `method-name` of the `obj` using `args`
(define (send-f obj method-name . args)
  (match-define (object cls vals) obj)

  (let loop ([cls cls])
    (when (not cls)
      (error 'send "object does not implement method"))
    
    (match-define (class-info parent fields methods) cls)
    (if (hash-has-key? methods method-name)
        (let ()
          (define method (hash-ref methods method-name))
          ;; mathing the order:
          ;; for each field f in the class,
          ;; this extract corrsp field from the object's hash
          (define field-values
            (for/list ([f fields])
              (hash-ref vals f)))
          ;; then applies the method within the firld values
          ;; upon the given extra args
          (apply (apply method field-values) args))
        (loop parent))))

;; a syntactic wrapper that helps us not
;; use the quote around the method name
(define-syntax-rule (send o (m args ...))
  (send-f o 'm args ...))


;; creates a new object that belongs to the `class`
(define (new class . args)
  (match-define (class-info parent fields methods) class)

  (when (not (= (length args) (length fields)))
    (apply raise-arity-error 'new (length fields) args))
  
  (object class
          (for/fold ([acc (hash)])
                    ([field fields]
                     [arg args])
            (hash-set acc field arg))))
