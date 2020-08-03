
(let ([g (Y (lambda (funs)
              (lambda (name)
                (match name
                  ['even?
                   (lambda (n)
                     (let ([even? (funs 'even?)]
                           [odd?  (funs 'odd?)])
                       (if (= n 0) #t (odd?  (- n 1)))))]
                  ['odd?
                   (lambda (n)
                     (let ([even? (funs 'even?)]
                           [odd?  (funs 'odd?)])
                       (if (= n 0) #f (even? (- n 1)))))]))))])
  (let ([even? (g 'even?)]
        [odd?  (g 'odd?)])
    (even? 123)))


(rewrite (letfuns ([(f x) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            finish this part)))])
              (let ([f (g 'f)] ...)
                B)))

;; Simple version that works for single argument functions only:
(rewrite (letfuns ([(f x) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                              ['f (lambda (x)
                                    (let ([f (funs 'f)] ...)
                                      E))]
                              ...))))])
              (let ([f (g 'f)] ...)
                B)))
|#

;; A simple change makes it work with any arity functions:
(rewrite (letfuns ([(f x ...) E] ...) B)
         => (let ([g (Y (lambda (funs)
                          (lambda (name)
                            (match name
                              ['f (lambda (x ...)
                                    (let ([f (funs 'f)] ...)
                                      E))]
                              ...))))])
              (let ([f (g 'f)] ...)
                B)))
