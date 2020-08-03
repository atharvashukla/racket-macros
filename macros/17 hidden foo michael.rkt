#lang racket

(struct hidden [foo])

(define h (hidden "my-password"))

(provide h)

(hidden-foo h)