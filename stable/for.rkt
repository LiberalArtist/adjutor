#lang racket/base

(require (for-syntax racket/base
                     syntax/for-body
                     syntax/parse))

(provide for/fold/define
         for*/fold/define
         for/lists/define
         for*/lists/define)

(define-syntaxes {for/fold/define for*/fold/define}
  (let ()
    (define (make for?/derived-id)
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
                                    . rst))))]))
    (values (make #'for/fold/derived)
            (make #'for*/fold/derived))))

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


(define-syntaxes {for/lists/define for*/lists/define}
  (let ()
    ;; Why not expand to `for/lists`?
    ;;
    ;;  1. There is no /derived variant,
    ;;     so error reporting would be worse.
    ;;
    ;;  2. Workaround for
    ;;     https://github.com/racket/racket/issues/3375
    (define (make for?/derived-id)
      (syntax-parser
        #:track-literals
        [(_ :accum-ids clauses:for-clauses
            ;; let `split-for-body` check details:
            body-or-break ...)
         #:fail-when (attribute result-expr)
         "#:result option not supported"
         #:with (tmp ...) (generate-temporaries #'(name ...))
         #:with ((pre-body ...) (post-body ...))
         (split-for-body this-syntax #'(body-or-break ...))
         (quasisyntax/loc this-syntax
           (define-values [name ...]
             #,(quasisyntax/loc this-syntax
                 (#,for?/derived-id
                  #,this-syntax
                  ([name null] ...
                   #:result
                   (values (reverse name) ...))
                  clauses
                  pre-body ...
                  (let-values ([{tmp ...}
                                #,(syntax/loc this-syntax
                                    (let ()
                                      post-body ...))])
                    (values (cons tmp name) ...))))))]))
    (values (make #'for/fold/derived)
            (make #'for*/fold/derived))))
  




