#lang racket

(require rackunit)

#|

Implementing Objects
====================

- Creating objects 
- Properties
- Methods
- ...with arguments
- Dynamic dispatch
- The notion of `this`
- Some form of inheritance or code-reuse

“An object is what an object has”,
              and
“An object is what an object does”


Javascript way: A dictionary of property/value pairs


|#

;; "An  Object is  what  an object has"

;; An Object is a [Listof PropVal]
;; A PropVal is a (list Name Value)
;; A Name is a symbol
 
(define my-pos1 (list (list 'x 3) (list 'y 4)))
(define my-pos2 (list (list 'x 6) (list 'y 8)))

;; accessing an object, java has obj.prop, we have (dot obj prop)

;; dot :: [Object Name -> Value]
(define (dot obj prop)
  (cond
    [(empty? obj) (error prop "No such property found")]
    [(symbol=? (first (first obj)) prop) (second (first obj))]
    [else (dot (rest obj) prop)]))
 
; (check-expect (dot my-pos1 'x) 3)
; (check-expect (dot my-pos2 'y) 8)
; (check-error (dot my-pos2 'z) "z: No such property found")

;; "An Object is what an object does"

;; that's the whole point of interfaces in  java
;; we don't know the fields, we only know what behaviors
;; we can invoke on it

;; An Object is [Name -> Value]
;; A Name is a symbol

(define my-pos3
  (λ (prop)
    (cond
      [(symbol=? prop 'x) 3]
      [(symbol=? prop 'y) 4]
      [else (error prop "No such property found")])))
(define my-pos4
  (λ (prop)
    (cond
      [(symbol=? prop 'x) 6]
      [(symbol=? prop 'y) 8]
      [else (error prop "No such property found")])))


;; dot :: [Object Name -> Value]
;; (1st attempt)
(define (dot2 obj prop)
  (obj prop))
 
; (check-expect (dot2 my-pos1 'x) 3)
; (check-expect (dot2 my-pos2 'y) 8)
; (check-error (dot2 my-pos2 'z) "z: No such property found")


;; abstracting over the parts that differ: values,
;; and we get an object constructor

(define (make-pos x y)
  (λ (prop)
    (cond
      [(symbol=? prop 'x) x]
      [(symbol=? prop 'y) y]
      [else (error prop "No such property found")])))
 
(define my-pos5 (make-pos 3 4))
(define my-pos6 (make-pos 6 8))


;; Inheritence

;; a 3-d point
(define (make-3d-pos x y z)
  (λ (prop)
    (cond
      [(symbol=? prop 'x) x]
      [(symbol=? prop 'y) y]
      [(symbol=? prop 'z) z]
      [else (error prop "No such property found")])))
 
(define my-pos7 (make-3d-pos 3 4 12))
; (check-expect (dot my-pos7 'z) 12)
; (check-error (dot my-pos7 'w) "w: No such property found")


;; -- using 'x  and 'y from 2d point
(define (make-3d-pos2 x y z)
  (local ;; [(define 2dpos (make-pos x y))]
    ;; renaming it to super:
    [(define super (make-pos x y))]
    (λ (prop)
      (cond
        [(symbol=? prop 'z) z]
        [else (dot super prop)]))))
 
(define my-pos8 (make-3d-pos 3 4 12))
; (check-expect (dot my-pos8 'z) 12)
; (check-error (dot my-pos8 'w) "w: No such property found")


;; an object that fails when looking up every single property
(define failure
  (λ(prop)
    (error prop "No such property found")))

;; and it can  be super for every object
(define (make-pos-wf x y)
  (local [(define super failure)]
    (λ(prop)
      (cond
        [(symbol=? prop 'x) x]
        [(symbol=? prop 'y) y]
        [else (dot super prop)]))))

;; Methods


(define (make-pos-m x y)
  (local [(define super failure)]
    (λ(prop)
      (cond
        [(symbol=? prop 'x) x]
        [(symbol=? prop 'y) y]
        ;; NEW:
        [(symbol=? prop 'dist-to-0)
         (sqrt (+ (sqr x) (sqr y)))]
        [else (dot super prop)]))))
 
; (check-expect (dot my-pos5 'dist-to-0) 5)
; (check-expect (dot my-pos6 'dist-to-0) 10)

;; this works
(define mypos (make-pos-m 3 4))
(check-equal? (dot2  mypos 'dist-to-0) 5)


;; for 3d point, simply adding dist-to-0
;; easily implements overriding
;; due to the natural behavior of cond

(define (make-3d-pos-m x y z)
  (local [(define super (make-pos x y))]
    (λ(prop)
      (cond
        [(symbol=? prop 'z) z]
        ;; NEW:
        [(symbol=? prop 'dist-to-0)
         (sqrt (+ (sqr x) (sqr y) (sqr z)))]
        [else (dot super prop)]))))

