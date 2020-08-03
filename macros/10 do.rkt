#lang racket

;; (do ((var init update) ...) (test result ...) expr ...) 
;; "do allows a common restricted form of iteration to be expressed succinctly."

(define factorial
  (lambda (n)
    (do ([i n (- i 1)] [a 1 (* a i)]) ;; ([var1 init1 update1] [var2 init2 update2])
      ((zero? i) a))))              ;; (test result)



(define fibonacci
  (lambda (n)
    (if (= n 0)
        0
        ;; ([var1 init1 update1] [var2 init2 update2] [var3 init3 update3])
        (do ([i n (- i 1)]    ;; countdown from the nth fib number
             [a1 1 (+ a1 a2)] ;; 2nd num (update = 1st (prev num) + 2nd (curr num))
             [a2 0 a1])       ;; 1st num (update = 2nd num)
          ;; (test result)
          ((= i 1) a1)))))



(define-syntax my-do
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...) ;; pattern [bindings is general]
       ; --------------
       ;; but where are we getting this? >> !It's just a helper, floating?!
       (with-syntax ([((var val update) ...) ;; assuming there is an update (it's optional)
                      (map (lambda (b)
                             (syntax-case b ()
                               [(var val) #'(var val var)] ;; only var val has default var
                               [(var val update) #'(var val update)])) ;; normal
                           #'(binding ...))])
         ;; ------------
         #'(let doloop ([var val] ...)
             (if test
                 (begin (if #f #f) res ...)
                 (begin expr ... (doloop update ...)))))])))

;; how does it know to affect binding ...