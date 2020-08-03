#lang racket

#|
1. Define a form (define/trace id expr) that behaves like define,
   except that the initial value of the variable and all subsequent
   values assigned with set! are printed to (current-output-port).
   This will need the tools discussed in the first tspl4 link.
|#


(define-syntax define/trace
  (syntax-rules ()
    [(_ id expr)
     (begin
       (define xid expr)
       (define-syntax id
         (lambda (x)
           (syntax-case x (set!)
             [_ (identifier? x) #'xid]
             [(set! id e) ;; i'm not mutating the syntax, i'm trying to match the pattern!
              #'(begin
                  (set! id e)
                  (displayln id (current-output-port)))]))))]))
#;
(define-syntax define/trace
  (syntax-rules ()
    [(_ id expr) ;; matches (define/trace var 3)
     (begin
       (define xid expr) ;; new name!?
       (define-syntax id 
         (identifier-syntax ;; what's  the racket replacement for this?
          [the-id #'xid]
          #;[(set! the-id e)
             #'(begin
                 (set! the-id e)
                 (displayln the-id (current-output-port)))])))]))


;;  (define/trace id expr)


(define/trace var 3)
;; ==>
#;(begin
    (define var 3)
    (displayln var (current-output-port)))


(set! var 4)
;; ==>
#;(begin
    (set! var 4)
    (displayln var (current-output-port)))

;; a define-syntax not only wraps the defining position
;; but also makes a macro that searches for all  set! occurrences and
;; wraps them