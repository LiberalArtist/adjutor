#lang racket/base

(require racket/match
         racket/string
         racket/bytes
         static-rename
         (for-syntax racket/base
                     syntax/parse
                     racket/string
                     racket/bytes
                     ))

(module+ test
  (require rackunit
           rackunit/spec
           "define-star.rkt"
           syntax/macro-testing))

(provide rx px)

(define-for-syntax ((make-rx/px-transformer proc
                                            byte-proc
                                            default-stx
                                            runtime-stx)
                    stx)
  (syntax-parse stx
    ;; no arguments -> default
    [(_ (~optional (~seq #:handler handler:expr)))
     default-stx]
    ;; all literal strings -> compile-time
    [(_ (~alt (~optional (~seq #:handler handler:expr))
              (~seq arg:str))
        ...)
     #`#,(proc (string-join (syntax->datum #'(arg ...))
                            "")
               (λ (msg)
                 (raise-syntax-error #f msg stx)))]
    ;; all literal bytes -> compile-time
    [(_ (~alt (~optional (~seq #:handler handler:expr))
              (~seq arg:bytes))
        ...)
     #`#,(byte-proc (bytes-join (syntax->datum #'(arg ...))
                                #"")
                    (λ (msg)
                      (raise-syntax-error #f msg stx)))]
    ;; syntax error if there are both literal bytes and literal strings
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
    ;; runtime application with no #:handler
    [(_ arg:expr ...)
     #`(#,runtime-stx arg ...)]
    ;; runtime application with #:handler
    [(_ (~alt (~once (~seq #:handler handler:expr))
              (~seq arg:expr))
        ...)
     #`(#,runtime-stx #:handler handler arg ...)]
    ;; higher-order use
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



(define ((make-runtime-proc who string-proc bytes-proc default)
         handler args)
  ;; check handler is valid
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
  ;; args is allways a list
  (match args
    ;; nothing -> fast path
    ['() default]
    ;; single string -> fast path
    [(list (? string? str))
     (string-proc
      str
      (or handler
          (λ (msg)
            (raise-arguments-error
             who
             (format "contract violation;\n ~a" msg)
             "given"
             str))))]
    ;; single bytes -> fast path
    [(list (? bytes? bs))
     (bytes-proc
      bs
      (or handler
          (λ (msg)
            (raise-arguments-error
             who
             (format "contract violation;\n ~a" msg)
             "given"
             bs))))]
    ;; otherwise, either car is bad or cdr is not null
    [(cons a more-args)
     ;; check first arg, and determine if we have bytes or strings
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
     ;; check remaining args are same type as first
     (for ([i (in-naturals)]
           [arg (in-list more-args)]
           #:unless (ok? arg))
       (apply raise-argument-error
              who
              expected-str
              (add1 i)
              args))
     ;; make the regular expression
     (define str-or-bs
       (join args sep))
     (proc str-or-bs
           (or handler
               (λ (msg)
                 (raise-arguments-error
                  who
                  (format "contract violation;\n ~a" msg)
                  "given"
                  str-or-bs))))]))



(define-syntax define-runtime-proc
  ; this gets both a pretty name and the liberally-expanded define
  ; for keyword arg performance
  (syntax-parser
    [(_ name:id who:id string-proc:expr bytes-proc:expr default:expr)
     #`(begin
         (define proc
           (make-runtime-proc 'who
                              string-proc
                              bytes-proc
                              default))
         (define/renamed who (name #:handler [handler #f] . args)
           (proc handler args)))]))

(define-runtime-proc runtime-rx rx
  regexp byte-regexp #rx"")

(define-runtime-proc runtime-px px
  pregexp byte-pregexp #px"")


;                                  
;                                  
;                                  
;                                  
;    ;;                      ;;    
;    ;;                      ;;    
;  ;;;;;;;    ;;;     ;;   ;;;;;;; 
;    ;;     ;;   ;  ;;  ;    ;;    
;    ;;     ;    ;   ;       ;;    
;    ;;    ;;;;;;;;   ;;     ;;    
;    ;;     ;           ;;   ;;    
;     ;     ;;   ;  ;   ;     ;    
;      ;;;    ;;;    ;;;       ;;; 
;                                  
;                                  
;                                  
;                                  


(module+ test
  (define first-class-rx
    rx)
  (define first-class-px
    px)
  (context
   "no string/byte arguments"
   (context
    "static"
    (it "px"
        (check-equal? (px)
                      #px""))
    (it "rx"
        (check-equal? (rx)
                      #rx""))
    (context
     "with ignored #:handler"
     (it "px"
         (check-equal? (px #:handler (error 'should-ignore))
                       #px""))
     (it "rx"
         (check-equal? (rx #:handler (error 'should-ignore))
                       #rx""))))
   (context
    "first class"
    (it "rx"
        (check-equal? (first-class-rx)
                      #rx""))
    (it "px"
        (check-equal? (first-class-px)
                      #px""))
    (context
     "with #:handler"
     (it "rx"
         (check-equal? (first-class-rx #:handler values)
                       #rx""))
     (it "px"
         (check-equal? (first-class-px #:handler values)
                       #px"")))))
  (context
   "literal arguments"
   (context
    "single"
    (context
     "string"
     (it "rx"
         (check-equal? (rx "test")
                       #rx"test"))
     (it "px"
         (check-equal? (px "test")
                       #px"test"))
     (context
      "with ignored #:handler"
      (it "rx"
          (check-equal? (rx "test" #:handler (error 'should-ignore))
                        #rx"test"))
      (it "px"
          (check-equal? (px "test" #:handler (error 'should-ignore))
                        #px"test"))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (rx #"test")
                       #rx#"test"))
     (it "px"
         (check-equal? (px #"test")
                       #px#"test"))
     (context
      "with ignored #:handler"
      (it "rx"
          (check-equal? (rx #"test" #:handler (error 'should-ignore))
                        #rx#"test"))
      (it "px"
          (check-equal? (px #"test" #:handler (error 'should-ignore))
                        #px#"test")))))
   (context
    "multiple"
    (context
     "string"
     (it "rx"
         (check-equal? (rx "test" "a" "b" "c")
                       #rx"testabc"))
     (it "px"
         (check-equal? (px "test" "a" "b" "c")
                       #px"testabc"))
     (context
      "with ignored #:handler"
      (it "rx"
          (check-equal? (rx #:handler (error 'should-ignore)
                            "test" "a" "b" "c")
                        #rx"testabc"))
      (it "px"
          (check-equal? (px #:handler (error 'should-ignore)
                            "test" "a" "b" "c")
                        #px"testabc"))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (rx #"test" #"a" #"b" #"c")
                       #rx#"testabc"))
     (it "px"
         (check-equal? (px #"test" #"a" #"b" #"c")
                       #px#"testabc"))
     (context
      "with ignored #:handler"
      (it "rx"
          (check-equal? (rx #:handler (error 'should-ignore)
                            #"test" #"a" #"b" #"c")
                        #rx#"testabc"))
      (it "px"
          (check-equal? (px #:handler (error 'should-ignore)
                            #"test" #"a" #"b" #"c")
                        #px#"testabc"))))))
  (context
   "non-literal arguments"
   (def
     [foo "foo"]
     [bar "bar"]
     [baz "baz"]
     [foo-bs #"foo"]
     [bar-bs #"bar"]
     [baz-bs #"baz"])
   (context
    "single"
    (context
     "string"
     (it "rx"
         (check-equal? (rx foo)
                       #rx"foo"))
     (it "px"
         (check-equal? (px foo)
                       #px"foo"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (rx foo #:handler values)
                        #rx"foo"))
      (it "px"
          (check-equal? (px foo #:handler values)
                        #px"foo"))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (rx foo-bs)
                       #rx#"foo"))
     (it "px"
         (check-equal? (px foo-bs)
                       #px#"foo"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (rx foo-bs #:handler values)
                        #rx#"foo"))
      (it "px"
          (check-equal? (px foo-bs #:handler values)
                        #px#"foo")))))
   (context
    "multiple"
    (context
     "string"
     (it "rx"
         (check-equal? (rx foo bar baz)
                       #rx"foobarbaz"))
     (it "px"
         (check-equal? (px foo bar baz)
                       #px"foobarbaz"))
     (context
      "plus literal"
      (it "rx"
          (check-equal? (rx foo bar baz "1")
                        #rx"foobarbaz1"))
      (it "px"
          (check-equal? (px foo bar baz "1")
                        #px"foobarbaz1")))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (rx #:handler values
                            foo bar baz)
                        #rx"foobarbaz"))
      (it "px"
          (check-equal? (px #:handler values
                            foo bar baz)
                        #px"foobarbaz"))
      (context
       "plus literal"
       (it "rx"
           (check-equal? (rx #:handler values
                             foo bar baz "1")
                         #rx"foobarbaz1"))
       (it "px"
           (check-equal? (px #:handler values
                             foo bar baz "1")
                         #px"foobarbaz1")))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (rx foo-bs bar-bs baz-bs)
                       #rx#"foobarbaz"))
     (it "px"
         (check-equal? (px foo-bs bar-bs baz-bs)
                       #px#"foobarbaz"))
     (context
      "plus literal"
      (it "rx"
          (check-equal? (rx foo-bs bar-bs baz-bs #"1")
                        #rx#"foobarbaz1"))
      (it "px"
          (check-equal? (px foo-bs bar-bs baz-bs #"1")
                        #px#"foobarbaz1")))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (rx #:handler values
                            foo-bs bar-bs baz-bs)
                        #rx#"foobarbaz"))
      (it "px"
          (check-equal? (px #:handler values
                            foo-bs bar-bs baz-bs)
                        #px#"foobarbaz"))
      (context
       "plus literal"
       (it "rx"
           (check-equal? (rx #:handler values
                             foo-bs bar-bs baz-bs #"1")
                         #rx#"foobarbaz1"))
       (it "px"
           (check-equal? (px #:handler values
                             foo-bs bar-bs baz-bs #"1")
                         #px#"foobarbaz1")))))))
  (context
   "first class"
   (context
    "single"
    (context
     "string"
     (it "rx"
         (check-equal? (first-class-rx "foo")
                       #rx"foo"))
     (it "px"
         (check-equal? (first-class-px "foo")
                       #px"foo"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (first-class-rx "foo" #:handler values)
                        #rx"foo"))
      (it "px"
          (check-equal? (first-class-px "foo" #:handler values)
                        #px"foo"))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (first-class-rx #"foo")
                       #rx#"foo"))
     (it "px"
         (check-equal? (first-class-px #"foo")
                       #px#"foo"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (first-class-rx #"foo" #:handler values)
                        #rx#"foo"))
      (it "px"
          (check-equal? (first-class-px #"foo" #:handler values)
                        #px#"foo")))))
   (context
    "multiple"
    (context
     "string"
     (it "rx"
         (check-equal? (first-class-rx "foo" "bar" "baz")
                       #rx"foobarbaz"))
     (it "px"
         (check-equal? (first-class-px "foo" "bar" "baz")
                       #px"foobarbaz"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (first-class-rx "foo" "bar" "baz" #:handler values)
                        #rx"foobarbaz"))
      (it "px"
          (check-equal? (first-class-px "foo" "bar" "baz" #:handler values)
                        #px"foobarbaz"))))
    (context
     "bytes"
     (it "rx"
         (check-equal? (first-class-rx #"foo" #"bar" #"baz")
                       #rx#"foobarbaz"))
     (it "px"
         (check-equal? (first-class-px #"foo" #"bar" #"baz")
                       #px#"foobarbaz"))
     (context
      "with #:handler"
      (it "rx"
          (check-equal? (first-class-rx #"foo" #"bar" #"baz" #:handler values)
                        #rx#"foobarbaz"))
      (it "px"
          (check-equal? (first-class-px #"foo" #"bar" #"baz" #:handler values)
                        #px#"foobarbaz"))))))
  
  (context
   "error cases"
   (context
    "compile time"
    (define (make-syntax-error-pred rxp)
      (match-lambda
        [(exn:fail:syntax (regexp rxp) _ _)
         #t]
        [_ #f]))
    (context
     "bad regular expression syntax"
     (define close-paren-error?
       (make-syntax-error-pred
        #rx"missing closing parenthesis in pattern"))
     (it "rx and string"
         (check-exn close-paren-error?
                    (λ () (convert-syntax-error (rx "(")))))
     (it "px and bytes"
         (check-exn close-paren-error?
                    (λ () (convert-syntax-error (px #"("))))))
    (context
     "mixed literals"
     (define mixed-literals-error?
       (make-syntax-error-pred
        #rx"cannot mix string\\? and bytes\\? values"))
     (it "rx"
         (check-exn mixed-literals-error?
                    (λ () (convert-syntax-error (rx "a" #"b")))))
     (it "px"
         (check-exn mixed-literals-error?
                    (λ () (convert-syntax-error (px "a" #"b")))))
     (context
      "with ignored #:handler"
      (it "rx"
          (check-exn mixed-literals-error?
                     (λ () (convert-syntax-error
                            (rx "a" #"b" #:handler (error 'not-here))))))
      (it "px"
          (check-exn mixed-literals-error?
                     (λ () (convert-syntax-error
                            (px "a" #"b" #:handler (error 'not-here)))))))
     (context
      "with non-literal"
      (define foo "foo")
      (it "rx"
          (check-exn mixed-literals-error?
                     (λ () (convert-syntax-error (rx "a" foo #"b")))))
      (it "px"
          (check-exn mixed-literals-error?
                     (λ () (convert-syntax-error (px "a" foo #"b")))))
      (context
       "with ignored #:handler"
       (it "rx"
           (check-exn mixed-literals-error?
                      (λ () (convert-syntax-error
                             (rx "a" foo #"b" #:handler (error 'not-here))))))
       (it "px"
           (check-exn mixed-literals-error?
                      (λ () (convert-syntax-error
                             (px "a" foo #"b" #:handler (error 'not-here))))))))))
   (context
    "run time"
    (context
     "invalid #:handler"
     (define foo "foo")
     (it "rx"
         (check-exn #rx"invalid #:handler argument"
                    (λ () (rx foo #:handler 'bad))))
     (it "px"
         (check-exn #rx"invalid #:handler argument"
                    (λ () (px foo #:handler 'bad))))
     (context
      "wrong arity"
      (define broken-handler (λ () #f))
      (it "rx"
          (check-exn #rx"invalid #:handler argument"
                     (λ () (rx foo #:handler broken-handler))))
      (it "px"
          (check-exn #rx"invalid #:handler argument"
                     (λ () (px foo #:handler broken-handler))))))
    (context
     "bad first argument"
     (it "rx"
         (check-exn #rx"\\(or/c string\\? bytes\\?\\).*argument position: 1st"
                    (λ () (rx #f "example"))))
     (it "px"
         (check-exn #rx"\\(or/c string\\? bytes\\?\\).*argument position: 1st"
                    (λ () (px #f "example")))))
    (context
     "bad subsequent argument"
     (it "rx"
         (check-exn #rx"expected: string\\?.*given: #f.*argument position: 2nd"
                    (λ () (rx "example" #f))))
     (it "px"
         (check-exn #rx"expected: bytes\\?.*given: #f.*argument position: 2nd"
                    (λ () (px #"example" #f)))))
    (context
     "mixed strings and bytes"
     (def [str "str"] [bs #"bs"])
     (context
      "expected string"
      (it "rx"
          (check-exn #rx"expected: string\\?.*given: #\"bs\".*argument position: 2nd"
                     (λ () (rx str bs))))
      (it "px"
          (check-exn #rx"expected: string\\?.*given: #\"bs\".*argument position: 2nd"
                     (λ () (px str bs)))))
     (context
      "expected bytes"
      (it "rx"
          (check-exn #rx"expected: bytes\\?.*given: \"str\".*argument position: 2nd"
                     (λ () (rx bs str))))
      (it "px"
          (check-exn #rx"expected: bytes\\?.*given: \"str\".*argument position: 2nd"
                     (λ () (px bs str))))))
    (context
     "bad regular expression syntax"
     (def [open-s "("] [open-b #"("])
     (it "rx and string"
         (check-exn #rx"missing closing parenthesis in pattern"
                    (λ () (rx open-s))))
     (it "px and bytes"
         (check-exn #rx"missing closing parenthesis in pattern"
                    (λ () (px open-b))))
     (context
      "intercepted by #:handler"
      (define (handle v)
        'passed)
      (it "rx and bytes"
          (check-eq? (rx open-b #:handler handle)
                     'passed))
      (it "px and string"
          (check-eq? (px open-s #:handler handle)
                     'passed))))
    ))
  #|END module+ test|#)


