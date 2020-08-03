#lang racket

(require rackunit)
(require syntax/parse)
(require (for-syntax syntax/parse))

;; removed  extra args, also removed (var1 val1) pairs
;; removed send-message
(define-syntax (define-object stx)
  (syntax-parse stx
    [(define-object (name . varlist)
       ((var2 val2) ...))
     ;; =>
     #'(define name
         (lambda varlist
           (lambda (msg)
             ;; note: the quote on msg is important
             (cond [(equal? msg 'var2) (val2)]
                   ...
                   [else (error "invalid message")]))))])) 

;; ------------------------------------------------------------

;; an object representing a 2d posn
(define-object (2dpos x y)
  ((get-x (lambda () x))   ;; get x
   (get-y (lambda () y)))) ;; get  y


;; instances of the object
(define 2dp (2dpos 1 2))
(define origin2d (2dpos 0 0))

;; extracting:
(check-equal? (2dp 'get-x) 1)
(check-equal? (2dp 'get-y) 2)

#;; macros stepping shows that this is what 2dpos evaluated to:
(define 2dpos
  (lambda (x y)
    (lambda (msg)
      (cond
        [(equal? msg 'get-x) ((lambda () x))]
        [(equal? msg 'get-y) ((lambda () y))]
        [else (error "invalid message")]))))


;; To try out inheritance, let's try to make 3dpos inherit x, y of  2dpos:
(define 3dpos
  (lambda (x y z)              ;; added z param
    (let ([super (2dpos x y)]) ;; <-- how do I know what args to pass into this?
      (lambda (msg)           
        (cond
          [(equal? msg 'get-z) ((lambda () z))]
          [else (super msg)])))))

#;; our macro is  producing:
(define 3dposi
  (lambda (x y z)
    (let ([super (2dpos 4 5)])
      (lambda (msg)
        (cond [(equal? msg 'get-z) ((lambda () z))]
              [else (super msg)])))))

;; testing (function)
(define 3dp (3dpos 4 5 6))
(define origin3d (3dpos 0 0 0))

;; extracting (function)
(check-equal? (3dp 'get-x) 4)
(check-equal? (3dp 'get-y) 5)
(check-equal? (3dp 'get-z) 6)

;; now converting the function into a macro:
;; adding `-i` for  inheritance
(define-syntax (define-object-i stx)
  (syntax-parse stx
    #:literals (inherit)
    [(_ (name . varlist)
        ((var2 val2) ...)
        (inherit obji))
     ;; =>
     #'(define name
         (lambda varlist
           ;; arguments to obji: how many and  which ones?
           (let ([super (obji 4 5)])
             (lambda (msg)
               (cond [(equal? msg 'var2) (val2)]
                     ...
                     [else (super msg)])))))]))

;; use of the macro:
(define-object-i (3dposi x y z)
  ((get-z (lambda () z)))
  (inherit 2dpos))

;; testing
(define 3dpi (3dposi 4 5 6))
(define origin3di (3dposi 0 0 0))

;; extracting:
(check-equal? (3dpi 'get-x) 4)
(check-equal? (3dpi 'get-y) 5)
(check-equal? (3dpi 'get-z) 6)