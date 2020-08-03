#lang racket

(require rackunit) 
(require syntax/macro-testing)

;; importing `syntax-parse` only at phase level 1 using `for-syntax`
(require (for-syntax syntax/parse))

#|
Summary
-------

What?

Using define-first-order, we can create a function whose arg #
can be chekced at compile time. However this function cannot be
used as a value. 

How?

The first macro `define-first-order` collects all the details - 
the function name, args, arg-count, and the body. Then it creates
a sub-macro - the macro's name is same as that of the function. This
macro calls `enforce-first-order` (this is the compile-time macro)
which checks for the args, if everything is correct, it returns a 
syntax-object containing the "application"
|#



;; allows for static checking of "argument numbers" to function calls
;; a generates a macro that looks up the function applications and checks them
(define-syntax define-first-order
  ; `syntax-parser`: like syntax-parse, but produces a matching procedure.
  ; The procedure accepts a single argument, which should be a syntax object.
  (syntax-parser
    ;; (define-first-order (add2 n) (+ n 2)) should match
    [(_ (func args ...) body ...)
     ; `#:with syntax-pattern stx-expr`
     ; Evaluates the stx-expr in the context of all previous attribute bindings
     ; and matches it against the pattern. If the match succeeds, the pattern’s
     ; attributes are added to environment for the evaluation of subsequent side
     ; conditions.
     #:with arg-length (length (syntax->list #'(args ...)))
     #'(begin
         ;; a copy of `f`, but has the name `tmp`
         (define (temp-func args ...) body ...)

         ;; generate a macro that will find all `func`s 
         ;; and do the necessary check on the arg length
         (define-syntax func
           ; remember: `define-syntax` creates a transformer binding
           ; (a syntactic transformation procedure : Stx -> Stx)
           
           ;; enforce-first-order : Number Stx-Obj [Any ... -> Any]
           (enforce-first-order 'arg-length #'temp-func)))]))

;; `begin-for-syntax` shifts the phase level of each form by one
(begin-for-syntax
  ;; `n` is the Number of arguments
  ;; `runtime` is a SyntaxObject (as a copy of the function)
  (define (enforce-first-order n function-stx)
    ;; Stx -> Stx [this is in phase level 1? So this means my types are right?]
    (lambda (stx)
      (syntax-parse stx
        ;; using a function as a value in a list, then extracting it. Consider:
        ;; (define-first-order (my-identity x) x)
        ;; ((first `(,my-identity)) 1)
        [_:id (raise-syntax-error #f "cannot use first-order function as expression" stx)]
        ;; At function position, with arg ... (i.e. normal funcion application)
        [(_:id arg ...)
         ;; number of args in the application
         (define cnt (length (syntax->list #'(arg ...))))
         (when (not (= cnt n))
           (raise-syntax-error #f "wrong argument count" stx))
         ;; finally... actually using the function: unquoting to evaluate.
         #`(#,function-stx arg ...)]))))




 
;; a function that places its args in a list
(define-first-order (f x y) (list x y))

;; function works with 2 args
(check-equal? (f 1 2) (list 1 2))

;; using f with 3 instead of 2 arguments
(check-exn exn:fail:syntax? (lambda () (convert-syntax-error (when false (f 1 2 3)))))

;; using f at a higher order
(check-exn exn:fail:syntax? (lambda () (convert-syntax-error ((first (list f)) 1 2))))

