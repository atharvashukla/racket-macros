#lang racket

;; syntax-rules problems

(define (repeat/proc n thunk)
  (when (> n 0) (thunk) (repeat/proc (sub1 n) thunk)))

(define-syntax-rule (repeat N E)
  (repeat/proc N (lambda () E)))


(define-syntax-rule (repeat2 N E)
  (for i = 1 to N do E))