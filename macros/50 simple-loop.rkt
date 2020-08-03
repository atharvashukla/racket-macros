#lang racket


(define-syntax for
  (syntax-rules (= to do)
    [(for x = m to n do body ...)
     (letrec ([loop (lambda (x)
                      (when (<= x n)
                        body ...
                        (loop (+ x 1))))])
       (loop m))]))