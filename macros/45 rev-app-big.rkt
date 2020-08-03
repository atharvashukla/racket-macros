#lang racket
(define-syntax-rule (rev-app F E ...)
  (rev-app-helper F (E ...) () () ()))
(define-syntax rev-app-helper
  (syntax-rules ()
    ;; this rule does the reversing, collecting the reversed
    ;; sequence in the last part -- also make up new identifiers
    ;; and collect them in *both* directions (`X' is the straight
    ;; sequence of identifiers, `X*' is the reversed one, and `E*'
    ;; is the reversed expression sequence); note that each
    ;; iteration introduces a new identifier called `t'
    [(rev-app-helper F (E0 E ...) (X ...  ) (  X* ...) (   E* ...))
     (rev-app-helper F (   E ...) (X ... t) (t X* ...) (E0 E* ...))]
    ;; and this rule fires up when we're done with the reversal and
    ;; the generation
    [(rev-app-helper F () (x ...) (x* ...) (E* ...))
     (let ([x* E*] ...)
       (F x ...))]))

;; see that it works
(define (show x) (printf ">>> ~s\n" x) x)
(rev-app list (show 1) (show 2) (show 3))