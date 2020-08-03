#lang racket

(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname
         (let-syntax ([name (identifier-syntax xname)])
           (lambda formals form1 form2 ...)))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #'((let-syntax ([name (identifier-syntax xname)])
                   (lambda formals form1 form2 ...))
                  arg (... ...))]))))]))




;; String -> String
;; appends ao to str
(define-integrable add-ao
  (lambda (str)
    (string-append str "ao")))



(add-ao "h")
(displayln "----")
(map add-ao '("1"))