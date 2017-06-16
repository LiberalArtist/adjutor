#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     ))

(provide string-when
         list-when
         any->boolean
         values->list
         list->values
         define-alias
         define-aliases
         for/fold/define
         for*/fold/define
         )


(module+ test
  (require rackunit
           (submod "..")))

(begin-for-syntax
  (define (make-when-like-transformer default-stx)
    (syntax-parser
      [(_ (~describe #:opaque "test expression"
                     test:expr)
          (~describe #:opaque "body form"
                     body:expr)
          ...+)
       #`(cond [test body ...]
               [else #,default-stx])])))

(define-syntax string-when
  (make-when-like-transformer #'""))

(define-syntax list-when
  (make-when-like-transformer #''()))


(define (any->boolean x)
  (not (not x)))

(define-syntax values->list
  (syntax-parser
    [(_ body:expr ...+)
     #'(call-with-values (λ () body ...) list)]))

(define (list->values lst)
  (apply values lst))




(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ name:id orig:id)
     #`(define-syntax name (make-rename-transformer #'orig))]))

(define-syntax (define-aliases stx)
  (define-syntax-class pr
    #:description "binding pair"
    (pattern [name:id orig:id]))
  (syntax-parse stx
    [(_ (bind:pr ...+))
     #:fail-when (check-duplicate-identifier
                  (syntax->list #'(bind.name ...)))
     "duplicate binding identifier"
     #`(begin (define-alias bind.name bind.orig) ...)]))

(define-syntaxes {for/fold/define for*/fold/define}
  (let ()
    (define-syntax-class binding
      #:description "binding pair"
      (pattern (var:id rhs:expr)))
    (define-syntax-class distinct-bindings
      #:description "sequence of distinct binding pairs"
      (pattern (b:binding ...)
               #:fail-when (check-duplicate-identifier
                            (syntax->list #'(b.var ...)))
                           "duplicate variable name"
               #:with (var ...) #'(b.var ...)
               #:with (rhs ...) #'(b.rhs ...)))
    (define ((make-for/define derived-stx) stx)
      (syntax-parse stx
        [(_ accumulators:distinct-bindings
            clauses
            body ...+)
         (with-syntax ([orig stx])
           #`(define-values {accumulators.var ...}
               (#,derived-stx orig accumulators clauses
                              body ...)))]))
    (values (make-for/define #`for/fold/derived)
            (make-for/define #`for*/fold/derived))))