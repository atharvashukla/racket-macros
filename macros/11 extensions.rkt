#lang racket


;; let to lambda transformation
(define-syntax my-let
  (syntax-rules ()
    ;; a let with a binding is a lambda 
    ;; with variables as params of the
    ;; lambda and the values applied to it
    [(_ ((x e) ...) b1 b2 ...)
     ((lambda (x ...) b1 b2 ...) e ...)]))

;; let* to a set of nested lets
;; 
(define-syntax my-let*
  (syntax-rules ()
    ;; no bindings leads to a simple let body
    [(_ () b1 b2 ...) (let () b1 b2 ...)]
    ;; non empy list  of bindings lead to
    ;; a let wrapped around the `rest' of
    ;; the bindings in a recursive let*
    [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
     (let ([i1 e1])
       (let* ([i2 e2] ...) b1 b2 ...))]))

