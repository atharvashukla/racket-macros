#lang racket

(let ([x 1]
      [y 2])
  (let-syntax ([x (make-set!-transformer
                      (lambda (stx)
                        (syntax-case stx (set!)
                          ; Redirect mutation of x to y
                          [(set! id v) #'(set! y v)]
                          ; Normal use of x really gets x
                          [id (identifier? #'id)  #'x])))])
    (begin
      (set! x (λ (x) x))
      ; (x 5) ;; =>
      (list x y))))




;; (x 5) ((lambda (x) x) 5)