#lang racket

(require rackunit)
(require syntax/parse)
(require (for-syntax syntax/parse))

; Problem Set 3
; Shukla, Atharva

;---------------------------------------------------------------------------------------------------
;; [1]
;; loop, which has the following grammar:

;; Expression = ...
;;            | (loop Variable0 ((Variable1 Expression1) ...) Expression0 ...)

#|
It creates a recursive function, named Variable0, whose parameters are Variable1 ...,
whose body is Expression0 ..., and immediately applies it to the arguments Expression1 ...
The Variable0 function is not visible outside of the loop expression.
|#

(define-syntax (loop stx)
  
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))
  
  (define-syntax-class distinct-bindings
    #:description "sequence of distinct binding pairs"
    (pattern (b:binding ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(b.var ...)))
             "duplicate variable name"
             #:with (var ...) #'(b.var ...)
             #:with (rhs ...) #'(b.rhs ...)))
  
  (syntax-parse stx
    [(_ loop:id bs:distinct-bindings body ...+)
       #'(letrec ([loop (lambda (bs.var ...) body ...)])
           (loop bs.rhs ...))]))



(check-equal? (loop fact ((n 5)) (if (= n 1) 1 (* n (fact (- n 1))))) 120)
(check-equal? (loop y ((x 2)) (+ 1 x)) 3)
(check-equal? (loop lp ([a 1] [b 2]) (+ a b)) 3)

;; fixes:
;; exp0 exp0more .. . -> exp0 in the macro syntax
;; changed from syntax-case to syntax parse
;; added error checking code from mylet ex.



;---------------------------------------------------------------------------------------------------
;; [2]
;; all, which has the following grammar:

;;  Expression  = ...
;;              | (all Expression ...)

#|
If all of the expressions evaluate to a non-#false value, all produces the list of the results;
otherwise it produces #false.
|#

(define-syntax (all stx)
    (syntax-parse stx
      [(_ exp ...)
       #'(let ([exps (list exp ...)])
           (and (not (ormap false? exps))
                exps))]))

;; TODO make a version wher you don't evaluate the rest when first evals to false


(check-equal? (all (begin (displayln "foo") 1)  #t 'a) '(1  #t a))
(check-equal? (all (not true) true) #f)
(check-equal? (all '(not true) 'true) (list '(not true) 'true))


