#lang racket/base

(require racket/match
         racket/string
         racket/bytes
         (for-syntax racket/base
                     syntax/parse
                     racket/string
                     racket/bytes
                     ))

(provide rx px)

(define-for-syntax ((make-rx/px-transformer proc
                                            byte-proc
                                            default-stx
                                            runtime-stx)
                    stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:handler handler:expr)))
     default-stx]
    [(_ (~alt (~optional (~seq #:handler handler:expr))
              (~seq arg:str))
        ...)
     #`#,(proc (string-join (syntax->datum #'(arg ...))
                            "")
               (λ (msg)
                 (raise-syntax-error #f msg stx)))]
    [(_ (~alt (~optional (~seq #:handler handler:expr))
              (~seq arg:bytes))
        ...)
     #`#,(byte-proc (bytes-join (syntax->datum #'(arg ...))
                                #"")
                    (λ (msg)
                      (raise-syntax-error #f msg stx)))]
    [(_ (~alt (~optional (~seq #:handler handler:expr))
              (~between (~seq bs:bytes) 1 +inf.0)
              (~between (~seq s:str) 1 +inf.0)
              (~seq (~and other:expr
                          (~not (~or o-bs:bytes o-s:str)))))
        ...)
     (raise-syntax-error
      #f
      "cannot mix string? and bytes? values"
      stx)]
    [(_ arg:expr ...)
     #`(#,runtime-stx arg ...)]
    [(_ (~alt (~once (~seq #:handler handler:expr))
              (~seq arg:expr))
        ...)
     #`(#,runtime-stx #:handler handler arg ...)]
    [name:id
     runtime-stx]))


(define-syntaxes (rx px)
  (values (make-rx/px-transformer regexp
                                  byte-regexp
                                  #'#rx""
                                  #'runtime-rx)
          (make-rx/px-transformer pregexp
                                  byte-pregexp
                                  #'#px""
                                  #'runtime-px)))



(define-values (runtime-rx runtime-px)
  (let ()
    (define ((make string-proc bytes-proc default who)
             #:handler [handler #f] . args)
      (when handler
        (unless (and (procedure? handler)
                     (procedure-arity-includes? handler 1))
          (raise-arguments-error
           who
           "contract violation;\n invalid #:handler argument"
           "expected" (unquoted-printing-string
                       "(or/c #f (-> any/c any)")
           "given" handler
           "other arguments..." args)))
      (match args
        [(cons a more-args)
         (define-values (proc ok? expected-str join sep)
           (cond
             [(string? a)
              (values string-proc string? "string?" string-join "")]
             [(bytes? a)
              (values bytes-proc bytes? "bytes?" bytes-join #"")]
             [else
              (apply raise-argument-error
                     who
                     "(or/c string? bytes?)"
                     0
                     args)]))
         (for ([i (in-naturals)]
               [arg (in-list more-args)]
               #:unless (ok? arg))
           (apply raise-argument-error
                  who
                  expected-str
                  (add1 i)
                  args))
         (define str-or-bs
           (join args sep))
         (proc str-or-bs
               (or handler
                   (λ (msg)
                     (raise-arguments-error
                      who
                      (format "contract violation;\n ~a" msg)
                      "given"
                      str-or-bs))))]
        [_ default]))
    (values (make regexp byte-regexp #rx"" 'rx)
            (make pregexp byte-pregexp #px"" 'px))))