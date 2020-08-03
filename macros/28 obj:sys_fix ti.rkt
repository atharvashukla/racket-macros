#lang racket

(define-syntax send-message
  (syntax-rules ()
    [(_ obj msg arg ...)
     (obj 'msg arg ...)]))

(define-syntax define-object-i
  (syntax-rules ()
    ;; with everything
    [(_ (name . varlist)
        ((var1 val1) ...)
        ((var2 val2) ...)
        ('inherit iobj))
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
    ;; without fields and inheritance
    [(_ (name . varlist)
        ((var2 val2) ...))
     (define-object-i (name . varlist)
       ()
       ((var2 val2) ...))]
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
    ;; without fields
    [(_ (name . varlist)
        ((var2 val2) ...)
        ('inherit iobj))
     (define-object-i (name . varlist)
       ()
       ((var2 val2) ...)
       ('inherit iobj))]))


#|
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

|#


;; an object representing a 2d posn
(define-object-i (2d-pos x y)
  ((x (lambda () x))
   (y (lambda () y))))


;; an object representing a 3d posn
(define-object-i (3d-pos x y z)
  ((z  (lambda () z)))
  ('inherit 2d-pos))


((send-message (3d-pos 1 2 3) x) 'o)