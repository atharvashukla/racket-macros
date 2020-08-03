#lang racket


(define-syntax val
  (lambda (stx)
    (syntax-case stx ()
      [val (identifier? (syntax val)) (syntax (get-val))])))


(define-values (get-val put-val!)
  (let ([private-val 0])
    (values (lambda () private-val)
            (lambda (v) (set! private-val v)))))


(define-syntax val2
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [val2 (identifier? (syntax val2)) (syntax (get-val))]
         [(set! val2 e) (syntax (put-val! e))]))))

(define-syntax-rule (define-get/put-id id get put!)
    (define-syntax id
      (make-set!-transformer
       (lambda (stx)
         (syntax-case stx (set!)
           [id (identifier? (syntax id)) (syntax (get))]
           [(set! id e) (syntax (put! e))])))))