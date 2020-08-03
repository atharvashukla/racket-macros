#lang racket



(letrec ([x (begin (display "q") (display "r"))])
  (begin (display "s") x))





