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

