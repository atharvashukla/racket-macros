#lang racket

(define-syntax for
  (syntax-rules (= to do)
    [(for x = m to n do body ...)
     (let* ([m* m]
            [n* (let ([x m*]) n)])
       (letrec ([loop (lambda (x)
                        (when (<= x n*)
                          body ...
                          (loop (+ x 1))))])
         (loop m*)))]))

(for i = (read) to (* i 3) do (displayln i))


