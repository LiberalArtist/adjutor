#lang racket/base

(require racket/match
         (for-syntax racket/base
                     syntax/parse
                     ))

(provide in-value*-record?
         in-value*
         in-value*/expression
         in-value*/generator
         in-match
         )

(module+ test
  (require rackunit
           (submod "..")))

(struct in-value*-record (thunk maybe-count)
  #:property prop:sequence
  (λ (this)
    (thunk->do-sequence
     (in-value*-record-thunk this))))

(define (generator->record gen)
  (check-gen gen #f)
  (if (in-value*-record? gen)
      gen
      (in-value*-record gen #f)))

(define (next-pos pos)
  #f)

(define (thunk->do-sequence thunk)
  (define (pos->element pos)
    (thunk))
  (make-do-sequence
   (λ ()
     (values pos->element
             next-pos
             #t
             values
             #f
             #f))))

(module+ test
  (check-equal?
   (for/list ([(a b) (thunk->do-sequence
                      (λ () (values 1 2)))])
     (vector a b))
   '(#(1 2))
   "check low-level thunk->do-sequence"))

(define (check-gen x maybe-expected)
  (match x
    [(in-value*-record _ maybe-count)
     (when (and maybe-count maybe-expected)
       (unless (= maybe-count maybe-expected)
         (error 'in-value*/generator
                "~a;\n ~a\n  expected: ~e\n  given: ~e\n  in-value*-record?: ~e"
                "result arity mismatch"
                "given in-value*-record? will return the wrong number of results"
                maybe-expected
                maybe-count
                x)))]
    [thunk
     #:when (and (procedure? thunk)
                 (procedure-arity-includes? thunk 0))
     (void)]
    [x
     (raise-argument-error
      'in-value*/generator
      "(or/c in-value*-record? (-> any))"
      x)]))


(define-for-syntax (sequence-syntax->context stx default)
  (syntax-parse stx
    [[_ (name:id _ ...)]
     (syntax->datum #'name)]
    [_ default]))

(define-sequence-syntax in-value*/generator
  (λ () #'generator->record)
  (λ (stx)
    (syntax-parse stx
      #:context (sequence-syntax->context stx 'in-value*/generator)
      [[(rslt:id ...) (_ gen-expr:expr)]
       #`[(rslt ...)
          (:do-in
           ([(gen) gen-expr])
           (check-gen gen #,(length (syntax-e #'(rslt ...))))
           ()
           #t
           ([(rslt ...) ((match gen
                           [(in-value*-record thunk _)
                            thunk]
                           [_ gen]))])
           #t
           #f
           [])]])))

(module+ test
  (check-equal?
   (for/list ([(a b) (in-value*/generator
                      (λ () (values 1 2)))])
     (vector a b))
   '(#(1 2))
   "check in-value*/generator as immediate for form")
  (let ([seq (in-value*/generator
              (λ () (values 1 2)))])
    (check-equal?
     (for/list ([(a b) seq])
       (vector a b))
     '(#(1 2))
     "check in-value*/generator as expression")
    (check-pred in-value*-record?
                (apply in-value*/generator (list seq))
                "check in-value*-record? and higher-order in-value*/generator")
    (check-equal?
     (for/list ([(a b) (in-value*/generator seq)])
       (vector a b))
     '(#(1 2))
     "check in-value*/generator with nested in-value*/generator")))

(define (in-value*-proc . args)
  (in-value*-record
   (λ () (apply values args))
   (length args)))

(define-sequence-syntax in-value*
  (syntax-parser
    #:context 'in-value*
    [(_ rslt-expr:expr ...)
     #:with (tmp ...)
     (generate-temporaries #'(rslt-expr ...))
     #`(let ([tmp rslt-expr] ...)
         (in-value*-record
          (λ () (values tmp ...))
          #,(length (syntax-e #'(rslt-expr ...)))))]
    [name:id
     #'in-value*-proc])
  (λ (stx)
    (syntax-parse stx
      #:context (sequence-syntax->context stx 'in-value*)
      [[(rslt:id ...) (_ rslt-expr:expr ...)]
       #:fail-unless (equal? (length (syntax-e #'(rslt ...)))
                             (length (syntax-e #'(rslt-expr ...))))
       (format "expected ~e expressions but given ~e"
               (length (syntax-e #'(rslt ...)))
               (length (syntax-e #'(rslt-expr ...)))) 
       #`[(rslt ...)
          (:do-in
           ([(rslt ...) (values rslt-expr ...)])
           #t
           ()
           #t
           ()
           #t
           #f
           [])]])))

(module+ test
  (check-equal?
   (for/list ([(a b) (in-value* 1 2)])
     (vector a b))
   '(#(1 2))
   "check in-value* as immediate for form")
  (check-eq?
   (for/first ([() (apply in-value* '())])
     'ok)
   'ok
   "check in-value* first-class use")
  (let ([seq (in-value* 1 2)])
    (check-equal?
     (for/list ([(a b) seq])
       (vector a b))
     '(#(1 2))
     "check in-value* as expression")
    (check-equal?
     (for/list ([(a b) (in-value*/generator seq)])
       (vector a b))
     '(#(1 2))
     "check in-value* under in-value*/generator")))

(define-sequence-syntax in-value*/expression
  (syntax-parser
    #:context 'in-value*/expression
    [(_ body:expr)
     #`(in-value*-record (λ () body) #f)])
  (λ (stx)
    (syntax-parse stx
      #:context (sequence-syntax->context stx 'in-value*/expression)
      [[(rslt:id ...) (_ body:expr)]
       #`[(rslt ...)
          (:do-in
           ([(rslt ...) body])
           #t
           ()
           #t
           ()
           #t
           #f
           [])]])))


(module+ test
  (check-equal?
   (for/list ([(a b) (in-value*/expression (values 1 2))])
     (vector a b))
   '(#(1 2))
   "check in-value*/expression as immediate for form")
  (let ([seq (in-value*/expression (values 1 2))])
    (check-equal?
     (for/list ([(a b) seq])
       (vector a b))
     '(#(1 2))
     "check in-value*/expression as expression")
    (check-equal?
     (for/list ([(a b) (in-value*/generator seq)])
       (vector a b))
     '(#(1 2))
     "check in-value*/expression under in-value*/generator")))

(define-sequence-syntax in-match
  (λ (stx)
    (raise-syntax-error
     #f
     "only allowed immediatly inside a for clause"
     stx))
  (λ (stx)
    (syntax-parse stx
      #:context (sequence-syntax->context stx 'in-match)
      [[(rslt:id ...) (~and stx (_ val:expr pat ...+))]
       #`[(rslt ...)
          (:do-in
           ([(rslt ...) (match/derived
                         val
                         stx
                         [pat
                          (values rslt ...)]
                         ...)])
           #t
           ()
           #t
           ()
           #t
           #f
           [])]])))

(module+ test
  (check-equal?
   (for/list ([(a b c) (in-match
                        '(1 2 3)
                        (list (and a b) c)
                        (list a b c))])
     (vector a b c))
   '(#(1 2 3))
   "check in-match"))


