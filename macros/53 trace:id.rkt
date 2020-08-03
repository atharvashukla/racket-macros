#lang racket

;; use make-variable-transformer and define-integrable
;; to define the tracing

(define-syntax define-integrable
  (syntax-rules (lambda)
    [(_ name (lambda formals form1 form2 ...))
     (begin
       (define xname (lambda formals (begin (displayln "hay")) form1 form2 ...))
       (define-syntax name
         (lambda (x)
           (syntax-case x ()
             [_ (identifier? x) #'xname]
             [(_ arg (... ...))
              #`((lambda formals (begin (displayln "something")) form1 form2 ...)
                 arg
                 (... ...))]))))]))


;; String -> String
;; appends ao to str
(define-integrable add-ao
  (lambda (str)
    (string-append str "ao")))



(add-ao "h")
(displayln "----")
(map add-ao '("1"))

(require  racket/trace)

(define iiif  (λ (x) x))
(trace iiif)

(iiif 1)