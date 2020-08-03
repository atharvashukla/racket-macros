#lang racket

(require rackunit)

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

(define-syntax loop
  (lambda (stx)
    (syntax-case stx ()
      [(_ var0 ((var1 exp1) ...) exp0  exp0more ...)
       #'(letrec ([var0 (lambda (var1 ...) exp0  exp0more ...)])
           (var0 exp1 ...))])))

(check-equal? (loop fact ((n 5)) (if (= n 1) 1 (* n (fact (- n 1))))) 120)
(check-equal? (loop x ((x 2)) (+ 1 x)) 3)

;---------------------------------------------------------------------------------------------------
;; [2]
;; all, which has the following grammar:

;;  Expression  = ...
;;              | (all Expression ...)

#|
If all of the expressions evaluate to a non-#false value, all produces the list of the results;
otherwise it produces #false.
|#

(define-syntax all
  (lambda (stx)
    (syntax-case stx ()
      [(_ exp ...)
       #'(let ([exps (list exp ...)])
           (and (not (ormap false? exps))
                exps))])))


(check-equal? (all (begin (displayln "foo") 1)  #t 'a) '(1  #t a))
(check-equal? (all (not true) true) #f)
(check-equal? (all '(not true) 'true) (list '(not true) 'true))


(define-syntax all*
  (lambda (stx)
    (syntax-case stx ()
      [(_ exp ...)
       #'(let ([exps (list exp ...)])
           (and (not (ormap false? exps))
                exps))])))


(lambda (a b)
  (all (not a) b))

(lambda (a b)
  (let ([t1 (not a)])
    (and t1
        (let ([t2 b])
          (and t2 (list t1 t2))))))

;---------------------------------------------------------------------------------------------------
;; [3]
;; dispatch, which has this grammar:

;; Expression = ...
;;            | (dispatch Expression DClause ...)
;;            | (dispatch Expression DClause ... [orelse Expression])
;;
;; DClause = [(Identifier ...) Expression]

#|
It evaluates the first Expression to a symbol; if it produces anything else, dispatch raises an
error. Next dispatch checks whether the symbol occurs in any of the identifier lists (interpreted
as list of symbols) and evaluates all corresponding expressions. If the symbol does not show up in
any of the identifier lists and an orelse clause is present, the expression in that clause is
evaluated.
|#

(define-syntax dispatch
  (lambda (stx)
    (syntax-case stx ()
      ;; base case : no clauses
      [(_  e1)
       #'(symbol? e1) ;; fender :)
       #'(void)]
      ;; the "no-orelse" case with 1 or more elem
      [(_  e1 [(id ...) e2] [(id-more ...) e2more] ...)
       #'(symbol? e1)
       #'(if (member e1 '(id ...))
             (begin e2 (dispatch e1 [(id-more ...) e2more] ...))
             (dispatch e1 [(id-more ...) e2more] ...))]
      ;; the "orelse" case
      [(_  e1 [(id ...) e2] ... [orelse e3])
       #'(symbol? e1)
       #'(if (member e1 (flatten '((id ...) ...)))
             (dispatch e1 [(id ...) e2] ...)
             e3)]
      ;; the "no-orelse" case with 0 or more elem
      [(_  e1 [(id ...) e2])
       #'(symbol? e1)
       #'(when (member e1 '(id ...))
           e2)])))


(dispatch 'x)

(define (f x)
  (dispatch x
            [(x y) (displayln 0)]
            [(z x) (displayln 1)]
            [orelse (displayln 2)]))

(f 'x) ; Works
;; (f 'z) ; Works
;; (f 'y) ; Works
;; (f 'a) ; Works

;; For (f 'x), x shows up in both regular clauses, while for (f 'a) a does not show up at all.

;---------------------------------------------------------------------------------------------------
;; [4]
;; struct/con, which has this grammar:

;; Expression = ...
;;            | (struct/con Identifier ({Identifier : Identifier} ...))

#|
The difference to plain struct is that each field comes with a second identifier, to the right of :.
This identifier names a predicate.

The form creates a struct type definition whose constructor ensures that the respective field values
satisfy the named predicate.

Note Racket’s struct comes with guards. The point of this exercise is that you may use struct but
not its guard feature. Instead, override the struct constructor. See hints.

Hint (1) It is possible to set! a function name. 
     (2) Use begin to splice a sequence of code into a context.
|#

(define-syntax struct/con
  (lambda (stx)
    (syntax-case stx ()
      [(_ name ({field : pred} ...))
       #'(andmap (λ (f p) (p f)) (field ...) (pred ...))
       #'(name field ...)])))

; (define-struct posn (x y))
(struct/con posn  ({x : number?}  {y : number?}))
(check-equal? (posn-x (struct/con posn ({1 : number?} {2 : number?}))) 1)
(check-equal? (posn-y (struct/con posn ({1 : number?} {2 : number?}))) 2)
;; (check-equal? (struct/con posn ({1 : number?} {2 : number?})) (posn 1 2))

;---------------------------------------------------------------------------------------------------
;; [5]
;; define-rewrite-rule, which accepts a syntax pattern and a syntax template and then synthesizes an
;; appropriate combination of define-syntax and syntax-parse.

(define-syntax define-rewrite-rule
  (syntax-rules ()
    [(_ (name P ...) B)
     (define-syntax name
       (lambda (stx)
         (syntax-case stx ()
           [(name P ...) #'B])))]))

(define-rewrite-rule
  (loop-for-ever exp)
  ; —> 
  (local ((define (for-ever) (begin exp (for-ever)))) (for-ever)))

;; If you think a feature is better off implemented as a function, do so.

;---------------------------------------------------------------------------------------------------
