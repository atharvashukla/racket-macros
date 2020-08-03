#lang racket


#|
(a b c d e f g h i j)
(0 1 2 3 4 5 6 7 8 9)
=>
      tmp
       |
       v
      (b c d e f g h i j a)
from: (1 2 3 4 5 6 7 8 9 0)
         | | | | | | | | | value of vars transferring onto new vars
         v v v v v v v v v
      (a b c d e f g h i j)
to:   (1 2 3 4 5 6 7 8 9 0)
       ^
       |
    set tmp
|#

(define-syntax rotate
  (syntax-rules ()
    [(rotate first more ...)
     (shift-to (more ... first) (first more ...))]))
 
(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (second more-first ...) (to0 to ...))
     (let ([tmp second])
       (set! to more-first) ...
       (set! to0 tmp))]))


(let ([a 1]
      [b 2]
      [c 3])
  (displayln (list a b c))
  (rotate a b c)
  (displayln (list a b c)))


#|
(a1 b2 c3)
(shift-to a c ...)
(rotate a1 b2 c3)
==
(shift-to (c ... a) (a c ...))
(shift-to (b2 c3 a1) (a1 b2 c3)
==
(let ([tmp b2])
  (set! b2 c3) (set! c3 a1) ;; how does  3rd get first?
  (set! a1 b2))   ;;; because I pu it at the back as an arg

(list a1 b2  c3)  =is really= (b2 c3 a1) {as expected}
|#



