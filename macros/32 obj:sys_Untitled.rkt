#lang racket


;; SYNTAX
#;
(define-object (name var1 ...)
  ((var2 expr) ...)
  ((msg action) ...))
;; and
#;
(send-message object msg arg ...)


; define-object creates an object constructor that uses let* to bind
; local fields and letrec to define the exported procedures.  An
; object is itself a procedure that accepts messages corresponding
; to the names of the exported procedures.  The second pattern is
; used to allow the set of local fields to be omitted.
(define-syntax define-object
  (syntax-rules ()
    ;; name of the object and the
    [(_ (name . varlist)             ;; name and the arguments will be treated as a list
        ((var1 val1) ...)            ;; zero or more fields:
        ((var2 val2) ...))           ;; zero or more methods
     ;; =>
     (define name                    ;; the object is really a function
       (lambda varlist               ;; (lambda `(p1 p2 p3)`
         (let* ([var1 val1] ...)     ;; binds the fields
           (letrec ([var2 val2] ...) ;; binds the methods
             (lambda (msg . args)    
               (case msg
                 [(var2) (apply var2 args)]
                 ...
                 [else (error 'name "invalid message" (cons msg args))]))))))]
    ;; what if we don't want any fields in the macro? no need  to specify the empty list
    [(_ (name . varlist) ((var2 val2) ...)) ;; matched only the methods
     (define-object (name . varlist)        ;; takes in the  same stuff
       ()                                   ;; and put in an empty field list
       ((var2 val2) ...))]))                ;; ... followed by  the method list

; send-message abstracts the act of sending a message from the act
; of applying a procedure and allows the message to be unquoted.
(define-syntax send-message
  (syntax-rules ()
    [(_ obj msg arg ...)   ;; a message is application of msg and arg ... on an obj
     (obj 'msg arg ...)])) ;; this quotes the msg syntax and applies it


;; similar to Scheme's built-in pair object type,
;; except that to access or assign its fields requires sending it messages
(define-object (kons kar kdr)
  ((get-car (lambda () kar))
   (get-cdr (lambda () kdr))
   (set-car! (lambda (x) (set! kar x)))
   (set-cdr! (lambda (x) (set! kdr x)))))


(define p (kons 'a 'b))

