#lang racket

(require rackunit)
(require syntax/macro-testing)
(require (for-syntax syntax/parse racket/match))

(begin-for-syntax
  ;; we're defining a struct at compile time
  ;; called `first-order` which stores:
  ;; - argument length (Number)
  ;; - function        (Stx-Obj)
  (struct first-order [arg-length temp-func]))

;; a macro that takes a function definition
;; and converts it into a struct (defined at compile time)
(define-syntax define-first-order
  (syntax-parser ;; <- produces a matching proc of 1 arg
    ;; match a function definition
    [(_ (func args ...) body ...)
     ;; get length of args
     #:with arg-length (length (syntax->list #'(args ...)))
     #'(begin
         ;; make a copy of func
         (define (temp-func args ...) body ...)
         ;; every function name is turned into a macro that
         ;; expands to a struct containing its details
         (define-syntax func
           ;; struct : (first-order Number Stx-Obj)
           (first-order 'arg-length #'temp-func)))]))

;; defines a macro that looks for apply-first-order
;; and then searches for the struct. 
;; usage: (apply-first-order f 1)
(define-syntax apply-first-order
  (syntax-parser
    ;; we match the application of the function
    [(_ func:id args ...)
     ; using `match-define`, binds the names
     ; to the extracted value of func
     (match-define
       ;; pattern: the structure of the struct
       (first-order arg-length temp-func)
       ;; expression: the struct from the context
       ;; `syntax-local-value` extracts it
       (syntax-local-value #'func))
     ;; making sure args ... are the same as arg-length (from the struct)
     (when (not (= arg-length (length (syntax->list #'(args ...)))))
       (raise-syntax-error #f "wrong argument count" this-syntax))
     ;; returning a syntax object that would evaluate the function at runtime
     #`(#,temp-func args ...)]))

;; a function that places its args in a list
(define-first-order (f x y) (list x y))

;; function works with 2 args
(check-equal? (apply-first-order f 1 2) (list 1 2))

;; function with 3 args
(check-exn exn:fail:syntax?  (λ () (convert-syntax-error (apply-first-order f 1 2 3))))

;; using f with 3 instead of 2 arguments
; (when false (apply-first-order f 1 2 3))

;; using f at a higher order
;; according to the macro, `(first (list f))` needs to be a macro
#;
(apply-first-order (first (list f)) 1 2)

;; but this shouts that the use of f in the let binding
;; is illegal. Because it wouldn't know where to find it.
#;
(let ([extracted (first (list f))])
  (apply-first-order extracted 1 2))