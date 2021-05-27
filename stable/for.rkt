#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/for-body))

(provide for/fold/define
         for*/fold/define
         for/lists/define
         for*/lists/define)

(module+ unstable-lists-left
  ;; consider: list*s variants where user provides init-expr
  (provide for/list-left
           for*/list-left
           for/lists-left
           for*/lists-left
           for/lists-left/define
           for*/lists-left/define))


(define-simple-macro (define-for-variants [for/- for*/-]
                       body:expr ...
                       make-proc:expr)
  (define-syntaxes [for/- for*/-]
    (let ()
      body ...
      (let ([make make-proc])
        (values (make #'for/fold/derived)
                (make #'for*/fold/derived))))))

(define-for-variants [for/fold/define for*/fold/define]
  (λ (for?/derived-id)
    (syntax-parser
      #:track-literals
      [(_ (~and accum-bindings
                (~describe "parenthesized sequence of accumulator binding pairs"
                           ((~describe "accumulator binding pair"
                                       [var:id rhs:expr])
                            ...)))
          . rst)
       #:fail-when (check-duplicate-identifier
                    (syntax->list #'(var ...)))
       "duplicate accumulator variable name"
       (quasisyntax/loc this-syntax
         (define-values [var ...]
           #,(quasisyntax/loc this-syntax
               (#,for?/derived-id #,this-syntax
                                  accum-bindings
                                  . rst))))])))


;; list-related


(begin-for-syntax
  (define-syntax-class for-clauses
    #:description "parenthesized sequence of for clauses"
    #:attributes {}
    (pattern ((~describe #:opaque "for clause"
                         ;; let /derived check details:
                         _)
              ...)))
  (define-syntax-class accum-ids
    #:description "parenthesized sequence of accumulator names"
    #:attributes {[name 1] result-expr}
    (pattern (name:id ...
              (~optional (~seq #:result result-expr:expr)))
             #:fail-when (check-duplicate-identifier
                          (syntax->list #'(name ...)))
             "duplicate accumulator variable name")))

(define-for-syntax (wrap-list-loop-body orig-stx accum-ids body-stx)
  (define/syntax-parse (name:id ...)
    accum-ids)
  (define/syntax-parse (tmp:id ...)
    (generate-temporaries accum-ids))
  (define/syntax-parse ((pre-body ...) (post-body ...))
    (split-for-body orig-stx body-stx))
  (syntax->list
   #`(pre-body ...
      (let-values ([{tmp ...}
                    #,(syntax/loc orig-stx
                        (let ()
                          post-body ...))])
        (values (cons tmp name) ...)))))




(define-for-variants [for/lists/define for*/lists/define]
  ;; Why not expand to `for/lists`?
  ;;
  ;;  1. There is no /derived variant,
  ;;     so error reporting would be worse.
  ;;
  ;;  2. Workaround for
  ;;     https://github.com/racket/racket/issues/3375
  (λ (for?/derived-id)
    (syntax-parser
      #:track-literals
      [(_ :accum-ids clauses:for-clauses . body*)
       #:fail-when (attribute result-expr)
       "#:result option not supported"
       (quasisyntax/loc this-syntax
         (define-values [name ...]
           #,(quasisyntax/loc this-syntax
               (#,for?/derived-id #,this-syntax
                                  ([name null] ...
                                   #:result
                                   (values (reverse name) ...))
                                  clauses
                                  #,@(wrap-list-loop-body this-syntax
                                                          #'(name ...)
                                                          #'body*)))))])))



;; left

(define-syntax for?/lists-left/helper
  (syntax-parser
    #:track-literals
    [(_ orig-stx f/f/d:id :accum-ids . stuff)
     (syntax-parse #'stuff
       #:context #'orig-stx
       #:track-literals
       [(clauses:for-clauses . body*)
        (quasisyntax/loc #'orig-stx
          (f/f/d orig-stx
                 ([name null] ...
                  (~? (~@ #:result result-expr)))
                 clauses
                 #,@(wrap-list-loop-body this-syntax
                                         #'(name ...)
                                         #'body*)))])]))


(define-for-variants [for/list-left for*/list-left]
  (λ (f/f/d-id)
    (syntax-parser
      #:track-literals
      [(_ . stuff)
       #`(for?/lists-left/helper #,this-syntax #,f/f/d-id (acc) . stuff)])))

(define-for-variants [for/lists-left for*/lists-left]
  (λ (f/f/d-id)
    (syntax-parser
      #:track-literals
      [(_ accs:accum-ids . stuff)
       #`(for?/lists-left/helper #,this-syntax #,f/f/d-id accs . stuff)])))

(define-for-variants [for/lists-left/define for*/lists-left/define]
  (λ (f/f/d-id)
    (syntax-parser
      #:track-literals
      [(_ accs:accum-ids . stuff)
       #:fail-when (attribute accs.result-expr)
       "#:result option not supported"
       (quasisyntax/loc this-syntax
         (define-values [accs.name ...]
           (for?/lists-left/helper #,this-syntax #,f/f/d-id accs . stuff)))])))

