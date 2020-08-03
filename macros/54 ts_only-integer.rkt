#lang racket


(define (check-int maybe-int)
  (if (integer? maybe-int)
      maybe-int
      (error "this is not an integer")))

#;
(define-syntax only-integer
  (syntax-rules ()
    [(_ id expr)
     (begin
       (define tmp (check-int expr))
       (define-syntax id 
         (make-set!-transformer
          (lambda  (stx)
            (syntax-case stx (set!)
              [the-id (identifier? #'the-id) #'tmp]
              [(set! the-id e)
               #'(set! tmp (check-int e))])))))]))

(define (check-proc maybe-proc)
  (if (procedure? maybe-proc)
      maybe-proc
      (error "this is not a procedure")))

(define-syntax only-procedure
  (syntax-rules ()
    [(_ id expr)
     (begin
       (define tmp (check-proc expr))
       (define-syntax id 
         (make-set!-transformer
          (lambda  (stx)
            (syntax-case stx (set!)
              [the-id (identifier? #'the-id) #'tmp]
              [(set! the-id e)
               #'(set! tmp (check-proc e))]
              [(_ arg (... ...)) #'(tmp arg (... ...))])))))]))


;; (only-integer var 5)
;; =>
;; (begin (define tmp (check-int 5))
;;       ... <macro> ...)



;; (var 5 6)

;; (set! var 6)
;; (set! var 6.4)
;; =>
;; (set! tmp (check-int 6))

; (only-procedure var 5)
(only-procedure var (λ (x) 5))


(set! var (λ (x) x))


(var 6)