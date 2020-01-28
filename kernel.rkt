#lang racket/base

(require (only-in racket/contract integer-in)
         (for-syntax racket/base
                     syntax/parse))

(provide string-when
         string-unless
         list-when
         list-unless
         any->boolean
         values->list
         list->values
         define-alias
         define-aliases
         for/fold/define
         for*/fold/define
         for/lists/define
         for*/lists/define
         infix:
         ip-port-num/c)


(module+ test
  (require rackunit
           (submod "..")))

(define-syntax infix:
  (syntax-parser
    [(_ left:expr op:expr right:expr)
     #'(op left right)]
    #; ; consider this
    [(_ (~alt (~once (~seq #: rator:expr))
              (~seq rand:expr))
        ...)
     #`(rator rand ...)]))

(begin-for-syntax
  (define (make-when+unless-like-transformers default-stx)
    (values (syntax-parser
              [(_ (~describe #:opaque "test expression"
                             test:expr)
                  (~describe #:opaque "body form"
                             body:expr)
                  ...+)
               #`(cond [test body ...]
                       [else #,default-stx])])
            (syntax-parser
              [(_ (~describe #:opaque "test expression"
                             test:expr)
                  (~describe #:opaque "body form"
                             body:expr)
                  ...+)
               #`(cond [test #,default-stx]
                       [else body ...])]))))

(define-syntaxes {string-when string-unless}
  (make-when+unless-like-transformers #'""))

(define-syntaxes {list-when list-unless}
  (make-when+unless-like-transformers #''()))


(define (any->boolean x)
  (and x #true))

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



(define-syntaxes {for/lists/define for*/lists/define}
  (let ()
    (define ((make-for/lists for/lists-id) stx)
      (syntax-parse stx
        [(_ (name:id ...)
            (~describe "for clauses" clauses:expr)
            body:expr ...+)
         #:fail-when (check-duplicate-identifier
                      (syntax->list #'(name ...)))
         "duplicate variable name"
         #:with for/*?lists for/lists-id
         #`(define-values (name ...)
             (for/*?lists (name ...)
               clauses
               body ...))]))
    (values (make-for/lists #'for/lists)
            (make-for/lists #'for*/lists))))


(define ip-port-num/c
  (integer-in 0 65535))

