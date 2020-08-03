#lang racket

(define-syntax (orelse stx)
  (printf "Expanding ~s\n" stx)
  (syntax-case stx ()
    [(orelse x y) (syntax (printf "Running an orelse\n"))]))


(orelse 1 2)