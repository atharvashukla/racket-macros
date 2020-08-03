#lang racket

(require syntax/parse)
(require (for-syntax syntax/parse))

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

(define-syntax-class idlist
  #:description "a list of identifiers"
  (pattern (i:id ...)))

(define-syntax-class dclause
  #:description "DClause: (list (listof Identifiers) Expression)"
  (pattern (idl:idlist e:expr)))

(define-syntax-class dclauselist
  #:description "a list of DClause"
  (pattern (dcl:dclause ...)))



(define-syntax (dispatch stx)
  
    (syntax-parse stx
      ;; base case : no clauses
      [(_  e1)
       #'(symbol? e1)
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
           e2)]))





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

