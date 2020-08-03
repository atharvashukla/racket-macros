#lang racket


#|
2. Try Exercise 12.8.3 in the section at the second tspl4 link. This is tricky--
-no worries if you get to it and have no idea what to do! In that case, come by 
the lab and we can discuss it.
|#


#|

Exercise 12.8.3
It is often useful to describe one object in terms of another. For example, the second kons object
type could be described as the same as the first but with a password argument and different actions
associated with the set-car! and set-cdr! messages. This is called inheritance; the new type of ob-
ject is said to inherit attributes from the first. Modify define-object to support inheritance by a-
llowing the optional declaration (inherit object-name) to appear after the message/action pairs. Th-
is will require saving some information about each object definition for possible use in subsequent
object definitions. Conflicting argument names should be disallowed, but other conflicts should be
resolved by using the initialization or action specified in the new object definition.

|#


;;; Section 12.8. Defining Abstract Objects ;;;


(define z (lambda (x . y) x))
