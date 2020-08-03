#lang racket

(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname (lambda formals form1 form2 ...))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((lambda formals form1 form2 ...)
                 arg
                 (... ...))]))))]))



;; String -> String
;; appends ao to str
(define-integrable add-ao
  (lambda (str)
    (string-append str "ao")))



(add-ao "h")

;(map add-ao '("1"))