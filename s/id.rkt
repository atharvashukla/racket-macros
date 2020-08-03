#lang racket

(require syntax/parse)

;; id is one of;
;; - patter variable
;; - annotated pattern variable
;; - literal


;; LITERAL
;; behaves like (~literal id)


(syntax-parse #'(define x 12)
  #:literals (define)
  [(define var:id body:expr) 'ok])

;; define is a literal
;; matches on define x 12,
;; the var is an id, returns 'ok

#;
(syntax-parse #'(lambda x 12)
  #:literals (define)
  [(define var:id body:expr) 'ok])

;; this does not match
;; becuase we expected a `define'
;; instead of the lambda

(syntax-parse #'(define x 12)
  #:literals ([def define])
  [(def var:id body:expr) 'ok])

;; interesting ^
;; we bind a literal define to def
;; and then use `def' to match define

#;
(syntax-parse #'(lambda x 12)
  #:literals ([def define])
  [(def var:id body:expr) 'ok])

;; we bound the define literal to a def
;; but still, lambda will not be matched



;; ANNOTATED PATTERN VARIABLE
;; behaves like (~var pvar-id syntax-class-id)


(syntax-parse #'a
  [var:id (syntax-e #'var)])

;; matched on a syntax-quoted a. it is an id
;; so var:id makes sure it is an id.
;; then we extract the "symbol" part using
;; syntax-e on the syntax object

#;
(syntax-parse #'12
  [var:id (syntax-e #'var)])

;; 12 is not an identifier.
;; this should error out.

(define-syntax-class two
  #:attributes (x y)
  (pattern (x y)))

;; tihs is a syntax-class
;; it has this "pattern" thing
;; two attributes: x, y. Can we extract them?
;; what are attributes used for?


(syntax-parse #'(a b)
  [t:two (syntax->datum #'(t t.x t.y))])


;; here's a USE of it
;; makes sure t is a two using `:`

;; doc: syntax->datum strips the lexical information

;; the t patter variable is put into the template as is
;; so that should copy hte (a b). and the "attributes"
;; are used to extract parts of the syntax class we defined.
;; using the attributes, we should get '((a b) a b)


(syntax-parse #'(a b)
  [t
   #:declare t two
   (syntax->datum #'(t t.x t.y))])

;; this gives us the same result as the previous
;; example (with :), but now, we use
;; `#:declare t two` --- why is this necessary?
