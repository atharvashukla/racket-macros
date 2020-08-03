#lang racket

(require (for-syntax syntax/parse))


(define (check-proc maybe-proc)
  (if (procedure? maybe-proc)
      maybe-proc
      (error "this is not a procedure")))

;; (identifier? . -> . set!-transformer?)
(begin-for-syntax
  (define (apply-only-proc tmp)
    (make-set!-transformer
     (lambda  (stx)
       (syntax-parse stx #:literals (set!)
         [_:id tmp]
         [(set! the-id e)
          #`(set! #,tmp (check-proc e))]
         [(_ arg ...) #`(#,tmp arg ...)])))))

(define-syntax only-procedure
  (syntax-rules ()
    [(_ id expr)
     (begin
       (define tmp (check-proc expr))
       (define-syntax id (apply-only-proc (quote-syntax tmp))))]))

;; (only-procedure var 5)
 (only-procedure var (λ (x) 5))


 (set! var (λ (x) x))
;; (set! var #f)


(var 6)