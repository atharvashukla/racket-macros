#lang racket


(define-syntax send-message
  (syntax-rules ()
    [(_ obj msg arg ...)
     (obj 'msg arg ...)]))

(define 2d-pos
  (lambda (x y)
    (lambda (msg . args)
      (letrec ([x (lambda () x)]
               [y (lambda () y)])
        (letrec ([super (lambda (mesg . args) (error "No match found!"))])
          (cond
            [(equal? msg 'x) (apply x args)]
            [(equal? msg 'y) (apply y args)]
            [else (send-message super msg args)]))))))

(define 3d-pos
  (lambda (x y z)
    (let* ([z (lambda () z)])
      (letrec ([inherit 2d-pos])
        (lambda (msg . args)
          (letrec ([super (lambda (mesg . args) (error "No match found!"))])
            (cond [(equal? msg 'inherit) (apply inherit args)] [else (send-message super msg args)])))))))


(define g (2d-pos 2 4))
; (define f (3d-pos 1 2 3))
(send-message g x)