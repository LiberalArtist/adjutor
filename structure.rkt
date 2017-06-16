#lang racket/base

(require racket/stxparam
         racket/match
         racket/contract
         (for-meta 2 racket/base)
         (for-syntax racket/base
                     syntax/parse
                     racket/struct-info
                     racket/stxparam
                     racket/contract
                     adjutor/kernel
                     ))

(module+ test
  (require rackunit))

(provide struct/derived
         structure
         raw-constructor
         (for-syntax raw-match-transformation
                     ))

(begin-for-syntax
  (define-splicing-syntax-class constructor-name-clause
    #:description "#:constructor-name or #:extra-constructor-name clause"
    #:attributes {kw name}
    (pattern (~seq (~and kw #:constructor-name) name:id))
    (pattern (~seq (~and kw #:extra-constructor-name) name:id)))
  (define-splicing-syntax-class struct-option
    ;not including #:constructor-name or #:extra-constructor-name
    (pattern (~seq (~or #:mutable
                        #:omit-define-syntaxes
                        #:omit-define-values
                        #:transparent
                        #:prefab
                        #:authentic)))
    (pattern (~seq (~or #:super 
                        #:inspector 
                        #:auto-value 
                        #:guard 
                        #:name 
                        #:extra-name 
                        #:constructor-name 
                        #:extra-constructor-name 
                        #:reflection-name)
                   unary:expr))
    (pattern (~seq (~or #:property 
                        #:methods)
                   a:expr
                   b:expr)))
  (define-syntax-class field-spec
    #:description "field declaration"
    #:attributes {name}
    (pattern name:id)
    (pattern [name:id (~or (~optional #:mutable)
                           (~optional #:auto))
                      ...]))
  #|END begin-for-syntax|#)


(define-syntax (struct/derived stx)
  (syntax-parse stx
    [(_ (error-name:id error-body ...)
        struct-name:id
        (~optional super-name:id)
        field-specs:expr
        (~or (~optional name-clause:constructor-name-clause
                        #:defaults ([name-clause.kw #'#:constructor-name]
                                    [name-clause.name #'struct-name]))
             opt:struct-option)
        ...)
     #`(define-struct/derived (error-name error-body ...)
         #,(if (attribute super-name)
               #'(struct-name super-name)
               #'struct-name)
         field-specs
         name-clause.kw name-clause.name
         #,@(apply append
                   (map syntax-e
                        (syntax-e #'(opt ...)))))]
    [(_ (error-name:id error-body ...) bad-body ...)
     #`(define-struct/derived (error-name error-body ...)
         bad-body ...)]))

(module+ test
  (struct super ())

  (struct/derived (apples #:foo)
                  name super ()))

(define-syntax-parameter raw-constructor
  (λ (stx)
    (raise-syntax-error
     #f
     "illegal outside of the #:constructor clause of a structure form"
     stx)))

(begin-for-syntax
  (define-syntax-parameter raw-match-transformation
    (λ (stx)
      (raise-syntax-error
       #f
       "illegal outside of the #:match-expander clause of a structure form"
       stx))))

(begin-for-syntax
  (struct structure-info-record (info transformer match-expander)
    #:transparent
    #:property prop:procedure (struct-field-index transformer)
    #:property prop:struct-info (λ (this) (structure-info-record-info this))
    #:property prop:match-expander (struct-field-index match-expander)))


(define-syntax (structure stx)
  (define-splicing-syntax-class struct-option
    ;not #:constructor-name #:extra-constructor-name
    ;    #:name or #:extra-name
    (pattern (~seq (~or #:mutable
                        #:omit-define-syntaxes
                        #:omit-define-values
                        #:transparent
                        #:prefab
                        #:authentic)))
    (pattern (~seq (~or #:super 
                        #:inspector 
                        #:auto-value 
                        #:guard 
                        #:reflection-name)
                   unary:expr))
    (pattern (~seq #:methods 
                   (~describe #:opaque "a name for a generic interface" a:expr)
                   (~describe #:opaque "generic methods declarations"
                              [b:expr ...])))
    (pattern (~seq #:property 
                   a:expr
                   b:expr)))
  (define-syntax-class constructor-contract-expr
    #:description #f
    #:attributes {c}
    (pattern raw
             #:declare raw (expr/c #'contract?
                                   #:name "#:constructor-contract argument")
             #:with c #'raw.c))
  (syntax-parse stx
    [(_ struct-name:id
        (~optional (~describe #:opaque "super-id" super-name:id))
        (~describe "field declarations"
                   (field:field-spec ...))
        (~or (~optional (~seq #:constructor-contract
                              constructor-contract:constructor-contract-expr)
                        #:defaults ([constructor-contract.c #'procedure?]))
             (~optional (~seq #:constructor wraped-constructor-expr)
                        #:defaults ([wraped-constructor-expr #'raw-constructor]))
             (~optional (~seq #:match-expander new-match-proc)
                        #:defaults ([new-match-proc #'raw-match-transformation]))
             opt:struct-option)
        ...)
     #:with (this-raw-transformer
             this-raw-constructor
             make-this)
     (generate-temporaries (let ([name-val (syntax-e #'struct-name)])
                             (list (format "~a-raw-transformer" name-val)
                                   (format "~a-raw-constructor" name-val)
                                   (format "~a-contracted-constructor" name-val))))
     #`(begin (struct/derived
               #,stx
               struct-name #,@(list-when (attribute super-name)
                                (list #'super-name))
               (field ...)
               #:constructor-name this-raw-constructor
               #:name this-raw-transformer
               #,@(apply append
                         (map syntax-e
                              (syntax-e #'(opt ...)))))
              (define make-this
                (let ([struct-name (procedure-rename this-raw-constructor
                                                     'struct-name)])
                  (syntax-parameterize ([raw-constructor
                                         (make-rename-transformer #'struct-name)])
                    (define/contract struct-name
                      constructor-contract.c
                      wraped-constructor-expr)
                    struct-name)))
              (define-syntax struct-name
                (let ([this-raw-match-transformation
                       (syntax-parser
                         [(_ body (... ...))
                          #'(this-raw-transformer body (... ...))])])
                  (structure-info-record
                   (extract-struct-info (syntax-local-value #'this-raw-transformer))
                   (syntax-parser
                     [(_ body (... ...))
                      #'(make-this body (... ...))]
                     [_ #'make-this])
                   (syntax-parameterize ([raw-match-transformation
                                          (make-rename-transformer
                                           #'this-raw-match-transformation)])
                     (define/contract struct-name
                       (-> syntax? syntax?)
                       new-match-proc)
                     struct-name))))
              )]))

(module+ test
  (structure point (x y z)
    #:transparent
    #:constructor (λ ([x 0] [y 0] [z 0])
                    (raw-constructor x y z))
    #:constructor-contract (->* {}
                                {real? real? real?}
                                point?)
    #:match-expander (syntax-parser
                       [(_ x y z)
                        (raw-match-transformation #'(_ x y z))]
                       [(_ x y)
                        #'(point x y _)]
                       [(_ x)
                        #'(point x _ _)]
                       [(_)
                        #'(point _ _ _)]))
                     
  (check-equal? (match (point)
                  [(point x) x])
                0)
  )
;(provide (struct-out point))
;point struct:point point-raw-constructor3 point? point-z point-y point-x

