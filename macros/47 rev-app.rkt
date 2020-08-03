#lang racket


(define-syntax-rule (rev-app F E ...)
  (rev-app-helper F (E ...) ()))
(define-syntax rev-app-helper
  (syntax-rules ()
    ;; this rule does the reversing, collecting the reversed
    ;; sequence in the last part
    [(rev-app-helper F (E0 E ...) (E* ...))
     (rev-app-helper F (E ...) (E0 E* ...))]
    ;; and this rule fires up when we're done with the reversal
    [(rev-app-helper F () (E ...))
     (let ([x E] ...)
       (F x ...))]))