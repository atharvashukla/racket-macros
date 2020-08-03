#lang racket


(define-syntax for
  (syntax-rules (= to do step)
    [(for x = m to n do body ...)
     (for x = m to n step 1 do body ...)]
    [(for x = m to n step d do body ...)
     (let ([n* n]
           [m* m]
           [d* d])
       (letrec ([loop (lambda (x)
                        (when (<= x n*)
                          body ...
                          (loop (+ x d*))))])
         (loop m*)))]))

; (for i = 1 to 10 do (printf "i = ~s\n" i))
(for i = 1 to 10 step 2 do (printf "i = ~s\n" i))