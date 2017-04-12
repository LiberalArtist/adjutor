#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     syntax/define
                     ))

(provide define*
         def
         )

(module+ test
  (require rackunit
           (submod "..")))

(begin-for-syntax
  (define-syntax-class ident-with-default
    #:description "identifier with default"
    #:attributes (name)
    (pattern [name:id default:expr]))
  (define-splicing-syntax-class kw-arg
    #:description "keyword argument"
    #:attributes (name)
    (pattern (~seq kw:keyword name:id))
    (pattern (~seq kw:keyword opt:ident-with-default)
             #:with name #'opt.name))
  (define-splicing-syntax-class kw-formal
    #:description "function argument"
    #:attributes (name)
    (pattern (~seq name:id))
    (pattern (~seq opt:ident-with-default)
             #:with name #'opt.name)
    (pattern kw:kw-arg
             #:with name #'kw.name))
  (define-syntax-class kw-formals
    #:description "kw-formals"
    #:attributes (arg-ids)
    (pattern (~describe "rest-id" rest-id:id)
             #:with arg-ids #'(rest-id))
    (pattern (spec:kw-formal ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ...)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ...))
    (pattern (spec:kw-formal ...+ . (~describe "rest-id"
                                               rest-id:id))
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ... rest-id)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ... rest-id)))
  (define-syntax-class plain-function-header
    #:description "function header (not curried)"
    #:attributes (name arg-ids)
    (pattern (name:id spec:kw-formal ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ...)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ...))
    (pattern (name:id spec:kw-formal ... . (~describe "rest-id"
                                                      rest-id:id))
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ... rest-id)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ... rest-id)))
  (define-syntax-class function-header
    #:description "function header"
    #:attributes (name arg-ids)
    (pattern (name:id spec:kw-formal ...)
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ...)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ...))
    (pattern (head:function-header spec:kw-formal ...)
             #:with name #'head.name
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ...)))
             "duplicate argument identifier"
             #:with arg-ids #'head.arg-ids)
    (pattern (name:id spec:kw-formal ... . (~describe "rest-id"
                                                      rest-id:id))
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ... rest-id)))
             "duplicate argument identifier"
             #:with arg-ids #'(spec.name ... rest-id))
    (pattern (head:function-header spec:kw-formal ... . (~describe "rest-id"
                                                                   rest-id:id))
             #:with name #'head.name
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(spec.name ... rest-id)))
             "duplicate argument identifier"
             #:with arg-ids #'head.arg-ids))
  )


(define-syntax (define* stx)
  (define-syntax-class define-lhs
    #:description #f
    #:attributes (name)
    (pattern name:id)
    (pattern head:function-header
             #:with name #'head.name))
  (define-syntax-class seq_definitions+expressions
    #:description "parenthesized sequence of definitions and expressions"
    #:attributes (body-list)
    #:literals (quote quasiquote syntax quasisyntax)
    (pattern [(~and body:expr
                    (~not (~or quote quasiquote syntax quasisyntax)))
              ...]
             #:attr body-list (syntax-e #'(body ...))))
  (define-splicing-syntax-class with-clause
    #:description "#:with clause"
    #:attributes (body-list)
    ;#:no-delimit-cut
    (pattern (~seq #:with body:seq_definitions+expressions)
             #:attr body-list (attribute body.body-list)))
  (syntax-parse stx
    [(_ head:define-lhs with:with-clause rhs-part:expr ...+)
     (define-values {name-stx rhs-stx}
       (normalize-definition #`(define head rhs-part ...)
                             #'λ
                             #t
                             #t))
     (with-syntax ([name name-stx]
                   [rhs rhs-stx])
       #`(define name
           (let ()
             #,@(attribute with.body-list)
             rhs)))]
    [(_ head:define-lhs rhs-part:expr ...+)
     #`(define head (let () rhs-part ...))]))

#|
(define* ((plain) n)
  (random n))

(define* val
  12)

(define* other-val
  #:with [(define other-val
            42)
          (displayln "Defining other-val!")]
  other-val)

(define* ((fancy) n)
  #:with [(define base
            (random 100))]
  (+ n base))
|#

(module+ test
  (let ()
    (define* (add-to n)
      #:with [(define base
                (random 100))]
      (+ n base))
    (check-equal? (add-to 5)
                  (add-to 5))))
    

#|
Defining other-val!
> val
12
> other-val
42
> ((plain) 10)
9
> ((plain) 10)
2
> ((fancy) 10)
35
> ((fancy) 10)
35


#;(define* (bad a )
    #:with [quote]
    12)

(define ((foo bar) bar)
  bar)
|#

(define-syntax (def stx)
  (define-syntax-class def-clause
    #:description "def clause"
    #:attributes {lhs rhs}
    (pattern [lhs:id rhs:expr])
    (pattern [head:function-header
              rhs-part:expr ...+]
             #:do [(define-values {lhs-stx rhs-stx}
                     (normalize-definition
                      #`(define head rhs-part ...)
                      #'λ
                      #t
                      #t))]
             #:with lhs lhs-stx
             #:with rhs rhs-stx))
  (syntax-parse stx
    [(_ clause:def-clause ...)
     #`(begin (define clause.lhs clause.rhs) ...)]))



