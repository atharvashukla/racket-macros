#lang racket



(let ([x 6])
  (define-syntax-rule (m)
    x)
  (let ([x 5])
    (m)))



(let ([x 6])
  (define-syntax m
    (λ (stx)
      (syntax-case stx ()
        [(_) #'x])))
  (let ([x 5])
    (m)))

#;
(define-syntax-rule (if-it E1 E2 E3)
  (let ([it E1]) (if it E2 E3)))

#;
(define-syntax (if-it stx)
  (syntax-case stx ()
    [(if-it E1 E2 E3)
     #'(let ([it E1]) (if it E2 E3))]))


(define-syntax (if-it stx)
  (syntax-case stx ()
    [(if-it E1 E2 E3)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let ([it E1]) (if it E2 E3)))]))


(if-it 222 (println it) 111)