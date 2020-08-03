# struct-info

``` racket
#lang racket

(require rackunit)
(require syntax/macro-testing)

(require
  (for-syntax syntax/parse
              racket/struct-info
              racket/list
              racket/string))

(struct point [x y])

#|  Structure Type Transformer Binding
    ----------------------------------
    The struct form binds the name of a structure type as a transformer binding that records the
    other identifiers bound to the structure type, the constructor procedure, the predicate
    procedure, and the field accessor and mutator procedures. This information can be used during
    the expansion of other expressions via `syntax-local-value`.

    (descriptor
     constructor
     predicate
     (Listof accessors)
     (Listof field-mutator)
     super)
|#

(define-syntax (get stx)
  (syntax-parse stx
    ;; matches (get point-example1 point x)
    [(_ instance struct field)
     ;; ... and gets the values of (point-x point-example1) at compile time
     ;; - `extract-struct-info` : extracts the list form of the
     ;; structure type information represented by the struct.
     ;; - `syntax-local-value`: returns the transformer binding value of
     ;; the identifier id-stx in the context of the current expansion.
     (define s-info (extract-struct-info (syntax-local-value #'struct)))
     ;; extracting and converting the name of the struct to a string
     (define name (symbol->string (syntax-e (second s-info))))
     ;; getting a lsit of accessors
     (define accessors (fourth s-info))
     ;; putting all the accessors in a hash
     (define mapping
       (for/fold ([acc (hash)])
                 ([accessor accessors])
         ;; first, converting the name of the accessor from sym to str
         (define a-name (symbol->string (syntax-e accessor)))
         ;; then setting current hash - acc
         ;; with a mapping like:
         ;; #((y . point-y) (x . point-x))
         (hash-set acc
                   (string-trim a-name ;; extracts the accessor-field
                                (string-append name "-")
                                #:right? #f)
                   accessor)))
     ;; use the field to get the corresponding accessor
     (define acc (hash-ref mapping (symbol->string (syntax-e #'field)) #f))
     ;; ... if there is none (defaults to #f) then throw an error
     (when (not acc)
       (raise-syntax-error 'get (format "no such field: ~s" (syntax-e #'field))))
     ;; convert the acc back to stx (SEE)
     ;; can also use unquote
     (define/syntax-parse acc-stx acc)
     #'(acc-stx instance)]))

(define p (point 1 2))

(check-equal? (get p point x) 1)
(check-exn exn:fail:syntax? (lambda () (convert-syntax-error (when false (get p point s)))))

```
