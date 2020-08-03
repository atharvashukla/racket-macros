#lang racket
(require rackunit)

;; Exercise 12.8.3: The Scheme Programming Language, Fourth Editon
;; https://www.scheme.com/tspl4/examples.html#./examples:h8

;; It is often useful to describe one object in terms of another. For example,
;; the second kons object type could be described as the same as the first but
;; with a password argument and different actions associated with the set-car!
;; and set-cdr! messages. This is called inheritance; the new type of object is
;; said to inherit attributes from the first. Modify define-object to support
;; inheritance by allowing the optional declaration (inherit object-name) to
;; appear after the message/action pairs. This will require saving some
;; information about each object definition for possible use in subsequent
;; object definitions. Conflicting argument names should be disallowed, but
;; other conflicts should be resolved by using the initialization or action
;; specified in the new object definition.

;; ---

; define-object creates an object constructor that uses let* to bind
; local fields and letrec to define the exported procedures.  An
; object is itself a procedure that accepts messages corresponding
; to the names of the exported procedures.  The second pattern is
; used to allow the set of local fields to be omitted.
(define-syntax define-object
  (syntax-rules ()
    [(_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...))
     (define name
       (lambda varlist
         (let* ([var1 val1] ...)
           (letrec ([var2 val2] ...)
             (lambda (msg . args)
               (case msg
                 [(var2) (apply var2 args)]
                 ...
                 [else
                  (error 'name
                         "invalid message"
                         (cons msg args))]))))))]
    [(_ (name . varlist)
        ((var2 val2) ...))
     (define-object (name . varlist)
       ()
       ((var2 val2) ...))])) 

; send-message abstracts the act of sending a message from the act
; of applying a procedure and allows the message to be unquoted.
(define-syntax send-message
  (syntax-rules ()
    [(_ obj msg arg ...)
     (obj 'msg arg ...)]))


;; ---

;; First, Start with concrete examples:

;; The first version of kons (from the book, renaming to kons1)

(define-object (kons1 kar kdr)
  ((get-car (lambda () kar))
   (get-cdr (lambda () kdr))
   (set-car! (lambda (x) (set! kar x)))
   (set-cdr! (lambda (x) (set! kdr x))))) 

(define p (kons1 'a 'b))
(check-equal? (send-message p get-car) 'a)
(check-equal? (send-message p get-cdr) 'b)
(send-message p set-cdr! 'c)
(check-equal? (send-message p get-cdr) 'c)

;; The second version of kons (renaming to kons2) with passwords

(define-object (kons2 kar kdr pwd)
  ((get-car (lambda () kar))
   (get-cdr (lambda () kar))
   (set-car!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x))))
   (set-cdr!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x))))))
;; note:  changing `if` to when in kons2


(define p1 (kons2 'a 'b "magnificent"))
(send-message p1 set-car! 'c "magnificent")
(check-equal? (send-message p1 get-car) 'c)
(send-message p1 set-car! 'd "please")
(check-equal? (send-message p1 get-car) 'c)

(define p2 (kons2 'x 'y "please"))
(send-message p2 set-car! 'z "please")
(check-equal? (send-message p2 get-car) 'z)

#|
;; We want to add inheritence:
(define-object (kons3 kar kdr pwd)
  (;; (get-car (lambda () kar))
   ;; (get-cdr (lambda () kar))
   (set-car!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x))))
   (set-cdr!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x)))))
  ((inherit kons1))) ;; <-- adding this


(define p3 (kons3 'a 'b  "lisp"))
(send-message p3 set-car! 'l  "lisp")
(check-equal? (send-message p3 get-car) 'i)
(send-message p1 set-car! 's "idk")
(check-equal? (send-message p3 get-car) 'i)
(check-equal? (send-message p3 get-cdr) 'b)

;; ---

;; A second example - Posn:

;; an object representing a 2d posn
(define-object (2d-pos x y)
  ((x (lambda () x))
   (y (lambda () y))))


;; an object representing a 3d posn
(define-object (3d-pos x y z)
  ((z  (lambda () z)))
  ((inherit 2d-pos)))
|#

;; ---


(define-syntax define-object-i
  (syntax-rules ()
    ;; without inheritance
    [(_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...))
     (define name
       (lambda varlist
         (let* ([var1 val1] ...)
           (letrec ([var2 val2] ...)
             (lambda (msg . args)
               (letrec ([super (lambda (mesg . args)
                                 (error "No match found!"))])
                 (cond
                   [(equal? msg 'var2)  (apply var2 args)]
                   ...
                   [else (send-message super msg args)])))))))]
    ;; with everything
    [(_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...)
        (inherit iobj))
     (define name
       (lambda varlist
         (let* ([var1 val1] ...)
           (letrec ([var2 val2] ...)
             (lambda (msg . args)
               (letrec ([super iobj])
                 (cond
                   [(equal? msg 'var2) (apply var2 args)]
                   ...
                   [else (send-message super msg args)])))))))]
    ;; without fields 
    [(_ (name . varlist)
        ((var2 val2) ...))
     (define-object-i (name . varlist)
       ()
       ((var2 val2) ...))]
    ;; without fields
    [(_ (name . varlist)
        ((var2 val2) ...)
        ('inherit iobj))
     (define-object-i (name . varlist)
       ()
       ((var2 val2) ...)
       (inherit iobj))]))



(define-object-i (kons10 kar kdr)
  ((get-car (lambda () kar))
   (get-cdr (lambda () kdr))
   (set-car! (lambda (x) (set! kar x)))
   (set-cdr! (lambda (x) (set! kdr x))))) 


(define-object-i (kons9 kar kdr pwd)
  ()
  ((set-car!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x))))
   (set-cdr!
    (lambda (x p)
      (when (string=? p pwd)
        (set! kar x)))))
 (inherit kons10))

(define p9 (kons9 'a 'b "magnificent"))
(send-message p9 set-car! 'c "magnificent")
; (check-equal? (send-message p9 get-car) 'c)
; (send-message p9 set-car! 'd "please")
; (check-equal? (send-message p9 get-car) 'c)
; (check-exn exn:fail? (λ () (send-message  p9 sss 's)))