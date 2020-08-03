#lang racket

#;
(define-syntax-rule (rev-app F E ...)
  (let (reverse ([x E] ...))
    (F x ...)))



(define-syntax-rule (rev-app-disp F E ...)
  (begin (displayln (list E ...  F))
         (displayln E ...)))

