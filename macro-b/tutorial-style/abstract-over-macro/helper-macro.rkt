#lang racket


;; ----------------------------
;; How to abstract over macros?
;; ----------------------------


;; Consider this simple macro that has a
;; repeated sub-expression: `(- (add2 num1) 2)`.

(define (add2 num) (+ num 2))

(define-syntax (my-macro.v1 stx)
  (syntax-case stx ()
    [(_ num1)
     #'(- (add2 num1) 2)]
    [(_ num1 num2)
     #'(+ (- (add2 num1) 2) num2)]))

;; This is how we can abstract that out:

;; define a helper macro that parametrizes the num""
(define-syntax (help stx)
  (syntax-case stx ()
    [(_ n) #'(- (add2 n) 2)]))

;; and use it in the main macro:
(define-syntax (my-macro.v2 stx)
  (syntax-case stx ()
    [(_ num1)
     #'(help num1)]
    [(_ num1 num2)
     #'(+ (help num1) num2)]))


(equal? (my-macro.v2 3) (my-macro.v2 3))     ;; ==> #t
(equal? (my-macro.v1 7 3) (my-macro.v2 7 3)) ;; ==> #t