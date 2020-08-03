#lang racket


(define-syntax-rule (cons-stream x y)
  (cons x (lambda () y)))

(define stream? pair?)
(define null-stream null)
(define null-stream? null?)
(define stream-first first)
(define (stream-rest s) ((rest s)))


(define ones (cons-stream 1 ones))

;? why is (stream-first ones) not working?
;; Ahh, looks like a mistake in the notes! contract violation
;; for first and rest, they should be car and cdr

;; this is correct:
(define stream-car car)
(stream-car ones)

(define (stream-map f s)
  (if (null-stream? s)
    null-stream
    (cons-stream (f (stream-first s))
                 (stream-map f (stream-rest s)))))


(define (stream-map2 f s1 s2)
  (if (null-stream? s1)
    null-stream
    (cons-stream (f (stream-first s1) (stream-first s2))
                 (stream-map2 f (stream-rest s1)
                                (stream-rest s2)))))

(define ints (cons-stream 0 (stream-map2 + ones ints)))
