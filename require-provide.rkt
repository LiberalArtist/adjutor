#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     ))

(provide require-provide
         (for-syntax require-provide-transformer
                     simple-require-provide-transformer
                     require-provide-spec
                     phase-level
                     module-path
                     ))

(module syntax racket/base
  (require racket/contract
           )
  (provide (contract-out
            [struct require-provide-transformer
              ([proc (-> syntax?
                         (values syntax? syntax?))])]
            [struct simple-require-provide-transformer
              ([proc (-> syntax? syntax?)])]
            ))
  (struct require-provide-transformer (proc))
  (struct simple-require-provide-transformer (proc))
  #|END module syntax|#)
(require (for-syntax 'syntax))
(begin-for-syntax
  (define-syntax-class phase-level
    #:description "phase level"
    (pattern #f)
    (pattern ph:exact-integer))
  (define-syntax-class module-path
    #:literals {submod}
    (pattern mod:root-module-path)
    (pattern (submod (~or mod:root-module-path "." "..")
                     (~or pth:id "..") ...)))
  (define-syntax-class root-module-path
    #:description "root module-path"
    #:literals {quote lib file planet}
    (pattern mod:id)
    (pattern (quote mod:id))
    (pattern mod:str)
    (pattern (file mod:str))
    (pattern (lib part:str ...+))
    (pattern planet-mod:planet-module-path))
  (define-syntax-class planet-module-path
    #:description "PLaneT module-path"
    #:literals {planet}
    (pattern (planet mod:id))
    (pattern (planet mod:str))
    (pattern (planet rel:str
                     (user:str pkg:str vers:planet-vers-spec)
                     part:str ...)))
  (define-splicing-syntax-class planet-vers-spec
    #:description "version for PLaneT module-path"
    (pattern (~seq nat:exact-nonnegative-integer))
    (pattern (~seq nat:exact-nonnegative-integer
                   minor:planet-minor-vers)))
  (define-syntax-class planet-minor-vers
    #:description "minor version for PLaneT module-path"
    #:datum-literals {= + -}
    (pattern nat:exact-nonnegative-integer)
    (pattern (x:exact-nonnegative-integer
              y:exact-nonnegative-integer))
    (pattern (= nat:exact-nonnegative-integer))
    (pattern (+ nat:exact-nonnegative-integer))
    (pattern (- nat:exact-nonnegative-integer)))
  (define-syntax-class derived-spec
    #:description "derived require-provide spec"
    #:attributes {require-stx provide-stx}
    (pattern (~and form (transform-stx:id subform ...))
             #:fail-unless (require-provide-transformer?
                            (syntax-local-value #'transform-stx
                                                (λ () #f)))
             #f ;"not a require-provide-transformer?"
             #:do [(define transform
                     (require-provide-transformer-proc
                      (syntax-local-value #'transform-stx)))
                   (define-values {req prov}
                     (transform #'form))]
             #:with require-stx req
             #:with provide-stx prov)
    (pattern (~and form (transform-stx:id subform ...))
             #:fail-unless (simple-require-provide-transformer?
                            (syntax-local-value #'transform-stx
                                                (λ () #f)))
             #f ;"not a simple-require-provide-transformer?"
             #:do [(define transform
                     (simple-require-provide-transformer-proc
                      (syntax-local-value #'transform-stx)))
                   (define-values {req prov}
                     (let ([rslt-stx (transform #'form)])
                       (syntax-parse rslt-stx
                         [spec:require-provide-spec
                          (values #'spec.require-stx
                                  #'spec.provide-stx)]
                         [_ (raise-arguments-error
                             'simple-require-provide-transformer
                             "transformer result was not a require-provide spec"
                             '|original syntax| #'form
                             '|transformer result| rslt-stx)])))]
             #:with require-stx req
             #:with provide-stx prov))
  (define-syntax-class require-provide-spec
    #:description "require-provide spec"
    #:literals {only-in except-in prefix-in rename-in only-meta-in
                        except-out rename-out prefix-out
                        for-syntax for-template for-label for-meta}
    ;relative-in 
    #:attributes {require-stx provide-stx}
    (pattern spec:derived-spec
             #:with require-stx (syntax/loc #'spec spec.require-stx)
             #:with provide-stx #'spec.provide-stx)
    (pattern mod:module-path
             #:with require-stx #'mod
             #:with provide-stx #'(all-from-out mod))
    (pattern (only-in spec:require-provide-spec
                      (~and id-maybe-renamed
                            (~or bind:id
                                 [orig:id bind:id]))
                      ...)
             #:with require-stx
             #'(only-in spec.require-stx id-maybe-renamed ...)
             #:with provide-stx #'spec.provide-stx)
    (pattern (except-in spec:require-provide-spec
                        bind:id ...)
             #:with require-stx
             #'(except-in spec.require-stx bind ...)
             #:with provide-stx #'spec.provide-stx)
    (pattern (prefix-in prefix:id spec:require-provide-spec)
             #:with require-stx #'(prefix-in prefix spec.require-stx)
             #:with provide-stx #'spec.provide-stx)
    (pattern (rename-in spec:require-provide-spec
                        [orig:id bind:id] ...)
             #:with require-stx
             #'(rename-in spec.require-stx [orig bind] ...)
             #:with provide-stx #'spec.provide-stx)
    (pattern (only-meta-in phase:phase-level spec:require-provide-spec ...)
             #:with require-stx #'(only-meta-in phase spec.require-stx ...)
             #:with provide-stx #'(combine-out spec.provide-stx ...))
    (pattern (except-out spec:require-provide-spec bind:id ...)
             #:with require-stx #'spec.require-stx
             #:with provide-stx #'(except-out spec.provide-stx bind ...))
    (pattern (rename-out spec:require-provide-spec [orig:id export:id] ...)
             #:with require-stx #'spec.require-stx
             #:with provide-stx #'(combine-out (except-out spec.provide-stx
                                                           orig ...)
                                               (rename-out [orig export] ...)))
    (pattern (prefix-out prefix:id spec:require-provide-spec)
             #:with require-stx #'spec.require-stx
             #:with provide-stx #'(prefix-out prefix spec.provide-stx))
    (pattern (for-syntax spec:require-provide-spec ...)
             #:with require-stx #'(for-syntax spec.require-stx ...)
             #:with provide-stx #'(for-syntax spec.provide-stx ...))
    (pattern (for-template spec:require-provide-spec ...)
             #:with require-stx #'(for-template spec.require-stx ...)
             #:with provide-stx #'(for-template spec.provide-stx ...))
    (pattern (for-label spec:require-provide-spec ...)
             #:with require-stx #'(for-label spec.require-stx ...)
             #:with provide-stx #'(for-label spec.provide-stx ...))
    (pattern (for-meta phase:phase-level spec:require-provide-spec ...)
             #:with require-stx #'(for-meta phase spec.require-stx ...)
             #:with provide-stx #'(for-meta phase spec.provide-stx ...))
    #|END require-provide-spec|#)
  #|END begin-for-syntax|#)

(define-syntax (require-provide stx)
  (syntax-parse stx
    [(_ spec:require-provide-spec ...)
     (syntax/loc stx
       (begin (require spec.require-stx ...)
              (provide spec.provide-stx ...)))]))

#;(module+ main

  #;(require-provide (except-in net/cookies/server
                                make-cookie))

  #;(require-provide (planet "foo" 42))
  #|
require-provide: expected root-module-path
  parsing context: 
   while parsing module-path
   while parsing require-provide spec in: (planet "foo" 42)
|#


  (define-syntax my-rpt
    (require-provide-transformer
     (syntax-parser
       [(_ name)
        (values #'name
                #'(all-from-out name))])))

  (define-syntax my-simple-rpt
    (simple-require-provide-transformer
     (syntax-parser
       [(_ name) #'name])))
  #|(define-syntax (show stx)
      (syntax-parse stx
        [(_ name:id)
         #`#,(format "~a" (syntax-local-value #'name))]))|#

  (require-provide (my-rpt libuuid))
  (require-provide (my-simple-rpt libuuid))

  #|(show my-rpt)

  (define-syntax (test stx)
    (syntax-parse stx
      [(_ name) #'(require name)]))

  (test libuuid)|#

  uuid-generate
    
  )