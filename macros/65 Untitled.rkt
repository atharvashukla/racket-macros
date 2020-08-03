#lang racket


(define-syntax for
  (syntax-rules (= to do)
    [(for x = m to n do body ...)
     (let ([n* n]
           [m* m]) ; execution order
       (letrec ([loop (lambda (x)
                        (when (<= x n*)
                          body ...
                          (loop (+ x 1))))])
         (loop m*)))]))

(for i = 1 to 3 do (printf "i = ~s\n" i))