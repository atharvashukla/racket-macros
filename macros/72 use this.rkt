(define (while condition-thunk body-thunk)
  (if (condition-thunk)
      (begin (body-thunk)
             (while condition-thunk body-thunk))
      (void)))


(let* ([i 0]
       [j (add1 i)])
  (while (λ () (< i 10))
         (λ () (begin (display i) (set! i j)))))


