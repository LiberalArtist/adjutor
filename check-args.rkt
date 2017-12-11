#lang racket/base

(require racket/contract
         syntax/location
         (for-syntax racket/base
                     adjutor/kernel
                     racket/match
                     syntax/parse
                     syntax/define
                     racket/syntax
                     ))

(module+ test
  (require rackunit
           syntax/macro-testing
           (submod "..")))

(provide define/check-args
         define/check-args/contract
         )

(begin-for-syntax
  (define-splicing-syntax-class kw-formal
    #:attributes (kind arg-name)
    (pattern (~seq arg-name:id)
             #:attr kind 'required)
    (pattern (~seq [arg-name:id default:expr])
             #:attr kind 'optional)
    (pattern (~seq kw-stx:keyword arg-name:id)
             #:attr kind (cons #'kw-stx 'required))
    (pattern (~seq kw-stx:keyword [arg-name:id default:expr])
             #:attr kind (cons #'kw-stx 'optional)))
  (define-syntax-class function-header
    #:attributes (kinds-list fn-name)
    #:description "function header"
    (pattern (fn-name:id spec:kw-formal ...)
             #:fail-when (check-duplicate-identifier
                          (attribute spec.arg-name))
             "duplicate argument identifier"
             #:attr kinds-list (attribute spec.kind))
    (pattern (fn-name:id spec:kw-formal ... . rest-arg:id)
             #:fail-when (check-duplicate-identifier
                          (cons #'rest-arg (attribute spec.arg-name)))
             "duplicate argument identifier"
             #:attr kinds-list (cons 'rest (attribute spec.kind)))
    (pattern (nested:function-header spec:kw-formal ...)
             #:fail-when (check-duplicate-identifier
                          (attribute spec.arg-name))
             "duplicate argument identifier"
             #:attr kinds-list (attribute nested.kinds-list)
             #:attr fn-name (attribute nested.fn-name))
    (pattern (nested:function-header spec:kw-formal ... . rest-arg:id)
             #:fail-when (check-duplicate-identifier
                          (cons #'rest-arg (attribute spec.arg-name)))
             "duplicate argument identifier"
             #:attr kinds-list (attribute nested.kinds-list)
             #:attr fn-name (attribute nested.fn-name)))
  (struct args-info (required optional required-kws optional-kws)
    #:prefab)
  (define (kinds-list->args-info kinds-list)
    (for/fold/define ([required 0]
                      [optional 0]
                      [required-kws '()]
                      [optional-kws '()])
                     ([kind (in-list kinds-list)])
      (match kind
        ['rest
         (values required +inf.0 required-kws optional-kws)]
        ['required
         (values (add1 required) optional required-kws optional-kws)]
        ['optional
         (values required (add1 optional) required-kws optional-kws)]
        [(cons kw-stx 'required)
         (values required optional (cons kw-stx required-kws) optional-kws)]
        [(cons kw-stx 'optional)
         (values required optional required-kws (cons kw-stx optional-kws))]))
    (args-info required optional required-kws optional-kws))
  #|END begin-for-syntax|#)


(define-syntax define/check-args
  (syntax-parser
    [(_ spec:function-header body:expr ...+)
     #:do [(define-values {name-stx rhs-stx}
             (normalize-definition #`(define spec body ...)
                                   #'λ
                                   #t
                                   #t))]
     #:with name name-stx
     #:with first-class-name (generate-temporary name-stx)
     #:with rhs rhs-stx
     (match-define (args-info required optional required-kws optional-kws)
       (kinds-list->args-info (attribute spec.kinds-list)))
     #`(begin
         ;macro must be defined first to work at the REPL
         (define-syntax (name stx)
           (syntax-parse stx
             [(_ (~alt (~between (~seq _:expr)
                                 #,required
                                 #,(+ required optional)
                                 #:name "by-position argument"
                                 #:too-few "too few by-position arguments"
                                 #:too-many "too many by-position arguments")
                       #,@(for/list ([kw-stx (in-list required-kws)])
                            (define datum (syntax->datum kw-stx))
                            #`(~once (~seq #,kw-stx _:expr)
                                     #:name #,(format "keyword argument ~a" datum)
                                     #:too-few #,(format "missing required keyword argument ~a" datum)
                                     #:too-many #,(format "illegally repeated keyword argument ~a" datum)))
                       #,@(for/list ([kw-stx (in-list optional-kws)])
                            (define datum (syntax->datum kw-stx))
                            #`(~optional (~seq #,kw-stx _:expr)
                                         #:name #,(format "keyword argument ~a" datum)
                                         #:too-many #,(format "illegally repeated keyword argument ~a" datum))))
                 (... ...))
              #`(first-class-name #,@(cdr (syntax-e stx)))]
             [first-class:id
              #'first-class-name]))
         (define first-class-name rhs))]))

(module+ test
  (define/check-args (foo reqd [opt #t] #:kw r-kw #:opt [opt-kw #t])
    'ok)

  (check-exn #rx"too few by-position arguments"
             (λ () (convert-syntax-error (foo))))

  (check-exn #rx"missing required keyword argument"
             (λ () (convert-syntax-error (foo 1))))
  (check-not-exn (λ () (foo 1 #:kw 2)))
  (check-exn #rx"illegally repeated keyword argument"
             (λ () (convert-syntax-error (foo 1 #:kw 2 #:kw 3)))
             "repeat required keyword arg")
  (check-not-exn (λ () (foo 1 #:kw 2 #:opt 3)))
  (check-exn #rx"illegally repeated keyword argument"
             (λ () (convert-syntax-error (foo 1 #:kw 2 #:opt 3 #:opt 4)))
             "repeat optional keyword argument")
  (check-not-exn (λ () (foo 1 #:kw 2 3)))
  (check-exn #rx"too many by-position arguments"
             (λ () (convert-syntax-error (foo 1 #:kw 2 3 4))))
  (check-not-exn (λ () foo)
                 "first-class use compiles")
  (check-exn #rx"application"
             (λ () (map foo '(1 2 3 4)))
             "bad higher-order use raises runtime error")
  
  (define/check-args (bar . args)
    args)

  (check-not-exn (λ () (bar))
                 "rest arg function w/ no args")
  (check-not-exn (λ () (bar 1 2))
                 "rest arg function w/ args")
  (check-exn #rx"bar"
             (λ () (convert-syntax-error (bar 1 #:kw #f)))
             "rest arg function rejects keywords")
  (check-not-exn (λ () (map bar '(1 2 3 4)))
                 "higher-order use works")

  (check-not-exn (λ ()
                   (define/check-args (recur arg)
                     (cond
                       [(pair? arg)
                        (println (car arg))
                        (recur (cdr arg))]
                       [else
                        arg]))
                   'ok)
                 "recursive use ok")

  (check-exn #rx"recur"
             (λ ()
               (convert-syntax-error
                (let ()
                  (define/check-args (recur arg)
                    (cond
                      [(pair? arg)
                       (println (car arg))
                       (recur)] ; error
                      [else
                       arg]))
                  'ok)))
             "static errors shown for recursive uses")
 
  #|END module+ test|#)



(define-syntax define/check-args/contract
  (syntax-parser
    [(_ spec:function-header cntct-expr body:expr ...+)
     #:declare cntct-expr (expr/c #'contract?
                             #:name "contract expression")
     #:do [(define-values {name-stx rhs-stx}
             (normalize-definition #`(define spec body ...)
                                   #'λ
                                   #t
                                   #t))]
     #:with name name-stx
     #:with first-class-name (generate-temporary name-stx)
     #:with rhs rhs-stx
     (match-define (args-info required optional required-kws optional-kws)
       (kinds-list->args-info (attribute spec.kinds-list)))
     #`(begin
         ;macro must be defined first to work at the REPL
         (define-syntax-rule (first-class/contracted effective-name srcloc)
           (contract cntct
                 first-class-name
                 '(definition effective-name)
                 (quote-module-name)
                 'effective-name
                 srcloc))
         (define-syntax (name stx)
           (syntax-parse stx
             [(effective-name:id
               (~alt (~between (~seq _:expr)
                                 #,required
                                 #,(+ required optional)
                                 #:name "by-position argument"
                                 #:too-few "too few by-position arguments"
                                 #:too-many "too many by-position arguments")
                       #,@(for/list ([kw-stx (in-list required-kws)])
                            (define datum (syntax->datum kw-stx))
                            #`(~once (~seq #,kw-stx _:expr)
                                     #:name #,(format "keyword argument ~a" datum)
                                     #:too-few #,(format "missing required keyword argument ~a" datum)
                                     #:too-many #,(format "illegally repeated keyword argument ~a" datum)))
                       #,@(for/list ([kw-stx (in-list optional-kws)])
                            (define datum (syntax->datum kw-stx))
                            #`(~optional (~seq #,kw-stx _:expr)
                                         #:name #,(format "keyword argument ~a" datum)
                                         #:too-many #,(format "illegally repeated keyword argument ~a" datum))))
                 (... ...))
              #`((first-class/contracted effective-name (quote-srcloc #,stx)) #,@(cdr (syntax-e stx)))]
             [effective-name:id
              #`(first-class/contracted effective-name (quote-srcloc #,stx))]))
         (define cntct cntct-expr.c)
         (define first-class-name rhs))]))

#;
[
(define/check-args/contract (num-identity x)
  (-> number? number?)
  x)

(module+ main
  (num-identity #f))
]
