#lang racket/base

(module reader racket/base
  (require syntax/module-reader
           syntax/strip-context
           syntax/parse)
  (provide (rename-out
            [test-read read]
            [test-read-syntax read-syntax]
            [test-get-info get-info]))
  (define ((wrap-reader/stx proc) . args)
    (syntax-parse
        (let ([rslt (apply proc args)])
          (if (syntax? rslt)
              rslt
              (datum->syntax #f rslt)))
      [(module:id name:id mod-lang:id body ...)
       (strip-context
        #`(module name (submod adjutor/test support)
            (module configure-runtime (submod adjutor/test support)
              (initialize-do-enter!))
            (module* test mod-lang
              body ...)))]))
  (define-values (test-read test-read-syntax test-get-info)
    (make-meta-reader
     'adjutor/test
     "language path"
     lang-reader-module-paths
     (λ (orig-read)
       (define new-read (wrap-reader/stx orig-read))
       (λ args
         (syntax->datum (apply new-read args))))
     wrap-reader/stx
     values)))



(module support racket/base
  (require syntax/location
           racket/enter
           (for-syntax racket/base
                       syntax/parse))
  (provide (except-out (all-from-out racket/base)
                       #%top-interaction)
           (rename-out
            [test-top-interaction #%top-interaction])
           initialize-do-enter!)
  (define-syntax-rule (initialize-do-enter!)
    (current-do-enter!
     (λ () (dynamic-enter! (quote-module-path ".." test)))))
  (define current-do-enter!
    (make-parameter void))
  (define (do-enter!)
    ((current-do-enter!)))
  (define-syntax test-top-interaction
    (syntax-parser
      [(_ . form)
       #`(begin (do-enter!) form)])))
    

