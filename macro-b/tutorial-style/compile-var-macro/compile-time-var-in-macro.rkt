#lang racket

;; ---------------------------------------------
;; How to use a compile time variable in a macro
;; ---------------------------------------------

(require (for-syntax syntax/parse))
(require rackunit)



;; here, I define `a-value` to be 12 at phase 1
(begin-for-syntax
  (define a-value 12))


;; a simple macro that swaps the application
(define-syntax (swap-app stx)
  (syntax-parse stx
    [(_ a b)
     ;; not that I'm putting a-value into my-value in a let
     ;; to illustrate that this is not within the #`
     ;; i.e this is executed @ compile time
     (let ([my-value a-value])
       ;; the value i want to use has to be unquoted:
       #`(+ (b a) #,my-value))]))


(check-equal? (swap-app 1 identity) 13)