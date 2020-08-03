#lang racket

;; named let


(define (duplicate pos lst)
  (let dup ([i 0]
            [lst lst])
   (dup (+ i 1) (cdr lst))))
 
; (duplicate 1 (list "apple" "cheese burger!" "banana"))


(let dup ([i 0]
          [lst '(1)])
   (begin (displayln i)
          (displayln lst)
          (displayln "------")
          (dup (+ i 1) `(car ,i lst))))