#lang racket/base

(provide infix:
         ip-port-num/c
         define-alias
         define-aliases
         any->boolean
         list->values
         values->list)

(require syntax/parse/define
         (only-in racket/tcp
                  [listen-port-number? ip-port-num/c])
         (for-syntax racket/base))

(module+ test
  (require rackunit
           (submod "..")))

(define-syntax-parser infix:
  #:track-literals
  [(_ left:expr op:expr right:expr)
   #'(op left right)]
  #; ; consider this
  [(_ (~alt (~once (~seq #: rator:expr))
            (~seq rand:expr))
      ...)
   #`(rator rand ...)])



(define (any->boolean x)
  (and x #true))

(define-simple-macro (values->list body:expr ...+)
  (call-with-values (λ () body ...) list))

(define (list->values lst)
  (apply values lst))



(define-simple-macro (define-alias name:id orig:id)
  (define-syntax name (make-rename-transformer #'orig)))

(define-syntax define-aliases
  (let ()
    ;; complicated for compatibility:
    ;; old documentation and implementation were inconsistent
    (define-syntax-class binding-pair
      #:description "binding pair"
      #:attributes {name orig}
      (pattern [name:id orig:id]))
    (syntax-parser
      #:track-literals
      [(_ (~or* (~describe "parenthesized sequence of binding pairs"
                           (:binding-pair ...+))
                (~seq :binding-pair ...+)))
       #:fail-when (check-duplicate-identifier
                    (syntax->list #'(name ...)))
       "duplicate binding identifier"
       #`(begin (define-alias name orig) ...)])))

