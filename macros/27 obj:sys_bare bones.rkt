#lang racket

(require rackunit)
(require syntax/parse)
(require (for-syntax syntax/parse))

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



;; an object representing a 2d posn

(define-object (2dpos x y)
  ((2dpos-x (lambda () x))   ;; get x
   (2dpos-y (lambda () y)))) ;; get  y


;; instances of the object
(define 2dp (2dpos 1 2))
(define origin (2dpos 0 0))

;; extracting:
(2dp '2dpos-x)(chec)
(check-equal? (2dp '2dpos-y) 2)