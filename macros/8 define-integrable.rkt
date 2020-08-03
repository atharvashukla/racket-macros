#lang racket


;; define-integrable
;; -----------------
;; similar to define for procedure definitions except that it causes the code
;; for the procedure to be integrated, or inserted, wherever a direct call
;; to the procedure is found.


(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname
         (let-syntax ([name (make-rename-transformer #'xname)])
           (lambda formals form1 form2 ...)))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((let-syntax ([name (identifier-syntax xname)])
                   (lambda formals form1 form2 ...))
                 arg (... ...))]))))]))


;; how could you use it?
;; in fact, how could you use the previous version?
(define-integrable fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1))))))




(define-integrable (rec x)
  (+ 1 (rec x)))
(rec 5)

(define (xname x)
  (+ 1 (xname x)))
(+ 1 (xname 5))

