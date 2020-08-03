#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse))



#| spec of mylet's syntax

(mylet ([var-id rhs-expr] ...) body ...+)
(mylet loop-id ([var-id rhs-expr] ...) body ...+)

|#

;; very simple implementation using define-syntax-rule
(define-syntax-rule (mylet ([var rhs] ...) body ...)
  ((lambda (var ...) body ...) rhs ...))


;; but leads to blithe errors
(mylet ([a 1] [b 2]) (+ a b)) ;; this is good
;; (mylet (b 2) (sub1 b)) ;; err
;; (mylet ([1 a]) (add1 a)) ;; err
;; 



#|
the macro writer benefits from the machine-checked specification of syntax in the form of more readable, maintainable code.
|#


;; same as the version above (but uses syntax-parse)
(define-syntax (mylet2 stx)
  (syntax-parse stx
    [(_ ([var-id rhs-expr] ...) body ...+)
     #'((lambda (var-id ...) body ...) rhs-expr ...)]))

(mylet2 ([a 1] [b 5]) (+ a b))


;; first step:
;; annotate with syntax classes
(define-syntax (mylet3 stx)
  (syntax-parse stx
    [(_ ([var-id:id rhs-expr:expr] ...) body:expr ...+)
     #'((lambda (var-id ...) body ...) rhs-expr ...)]))

(mylet3 ([a 1] [b 5]) (+ a b))
;; (mylet3 (["a" 1]) (add1 a)) ;; gives the right error message


;; syntax parse deals with some error properly
;; (mylet ([a 1 2]) (* a a)) ;; unexpected term at 2
;; however, does NOT deal with this properly
;; (mylet (a 1) (+ a 2))


;; it was expecting a "binding pair" at a's position. but that's not in its vocab yet.


(define-syntax (mylet4 stx)
  
    (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id rhs:expr)))
  
    (syntax-parse stx
      [(_ (b:binding ...) body ...+)
       #'((lambda (b.var ...) body ...) b.rhs ...)]))

;; note: we can use attributes var and rhs of a binding
;; like b.var, v.rhs


;; now error messages can talk about "binding pairs"
;; (mylet (a 1) (+ a 2))
;; ^ would say "expected a binding pair"







;; (mylet ([a 1] [a 2]) (+ a a))
;; this talks about duplicate arg to a lambda

;; need to express distinctness constraint as a side condition:
(define-syntax (mylet5 stx)
  
  (define-syntax-class binding
    #:description "binding pair"
    (pattern (var:id rhs:expr)))

  (syntax-parse stx
    [(_ (b:binding ...) body ...+)
     ;; NEW:
     ;; cheks duplicate ids in a list of ids
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(b.var ...))) ;; the condition
     "duplicate variable name"                  ;; the error msg
     ;; ---
     #'((lambda (b.var ...) body ...) b.rhs ...)]))



(define-syntax (mylet6 stx)
  
    (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id rhs:expr)))
  
    (define-syntax-class distinct-bindings
      #:description "sequence of distinct binding pairs"
      (pattern (b:binding ...)
               #:fail-when (check-duplicate-identifier
                            (syntax->list #'(b.var ...)))
               "duplicate variable name"
               ;; we bind `var' and `rhs' as attributes of distinct bindings
               #:with (var ...) #'(b.var ...)
               #:with (rhs ...) #'(b.rhs ...)))
  
  ;; note: a syntax class only exports its patterns’
  ;; pattern variables as attributes, not their nested attributes

  ;; in this case: distinct bindings exports b (a binding)
  ;; but not var and rhs of binding itself (which we specify using #:with)
  
    (syntax-parse stx
      [(_ bs:distinct-bindings . body)
       #'((lambda (bs.var ...) . body) bs.rhs ...)]))


;; implementing the named let


(define-syntax (mylet7 stx)
  
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
    [(_ bs:distinct-bindings body ...+)
     #'((lambda (bs.var ...) body ...) bs.rhs ...)]

    ;; NEW: loop is called in the body of letrec.
    ;; loop itself is bound using a lambda in the letrec
    [(_ loop:id bs:distinct-bindings body ...+)
     #'(letrec ([loop (lambda (bs.var ...) body ...)])
         (loop bs.rhs ...))]))


#|

The answer is that syntax-parse records a list of all the potential errors 
(including ones like loop not matching distinct-binding) along with the progress
made before each error. Only the error with the most progress is reported.

|#


