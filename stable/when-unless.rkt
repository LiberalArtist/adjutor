#lang racket/base

(provide string-when
         string-unless
         list-when
         list-unless)

(require (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit
           (submod "..")))

(define-for-syntax (make-when+unless-transformers default-stx)
  (define-syntax-class when/unless-form
    #:description #f
    #:attributes {test [body 1]}
    (pattern (_ (~describe #:opaque "test expression"
                           test:expr)
                (~describe #:opaque "body form"
                           body:expr)
                ...+)))
  (values (syntax-parser
            [:when/unless-form
             #`(cond [test body ...]
                     [else #,default-stx])])
          (syntax-parser
            [:when/unless-form
             #`(cond [test #,default-stx]
                     [else body ...])])))

(define-syntaxes {string-when string-unless}
  (make-when+unless-transformers #'""))

(define-syntaxes {list-when list-unless}
  (make-when+unless-transformers #''()))
