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
           syntax/macro-testing
           (submod "..")))

;; TODO: in-match can bind identifiers from the context of the expression
;;   that are not bound by the match clauses.
;;   Is that good or bad?
;;   If bad, can it be solved by adding extra scopes?

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

(begin-for-syntax
  (define-splicing-syntax-class bind-clause
    #:description "#:bind clause"
    (pattern (~seq #:bind [rslt:id ...]))))

(define-sequence-syntax in-match
  (λ (stx)
    (syntax-parse stx
      [(_ val:expr bind:bind-clause pat ...+)
       #'(let ([the-val val])
           (in-value*/expression
            (match/derived
             the-val
             #,stx
             [pat
              (values bind.rslt ...)]
             ...)))]))
  (λ (stx)
    (syntax-parse stx
      #:context (sequence-syntax->context stx 'in-match)
      [[(rslt:id ...) (~and rhs-stx (_ val:expr bind:bind-clause pat ...+))]
       (unless (equal? (length (syntax-e #'(rslt ...)))
                       (length (syntax-e #'(bind.rslt ...))))
         (raise-syntax-error
          (sequence-syntax->context stx 'in-match)
          "wrong number of results;\n number from #:bind clause does not match for-clause"
          stx
          #'rhs-stx))
       #`[(rslt ...)
          (:do-in
           ([(rslt ...) (match/derived
                         val
                         rhs-stx
                         [pat
                          (values bind.rslt ...)]
                         ...)])
           #t
           ()
           #t
           ()
           #t
           #f
           [])]]
      [[(rslt:id ...) (~and rhs-stx (_ val:expr pat ...+))]
       #`[(rslt ...)
          (:do-in
           ([(rslt ...) (match/derived
                         val
                         rhs-stx
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
   "check in-match")

  (test-case
   "in-match"
   (define-simple-check (check-in-match form)
     (equal? '(#(1 2 3))
             form))

   (check-in-match
    (for/list ([(a b c) (in-match '(1 2 3)
                                  (list a b c))])
      (vector a b c))
    "Use in for-clause without #:bind")

   (check-in-match
    (for/list ([(a b c) (in-match '(1 2 3)
                                  #:bind [a b c]
                                  (list a b c))])
      (vector a b c))
    "Use in for-clause with #:bind")

   (check-in-match
    (for/list ([(x y z) (in-match '(1 2 3)
                                  #:bind [a b c]
                                  (list a b c))])
      (vector x y z))
    "Use in for-clause with #:bind and different names")

   (check-not-exn
    (λ ()
      (in-match '(1 2 3)
                #:bind [a b c]
                (list a b c)))
    "Use outside a for-clause")

   (define ms-seq
     (in-match (current-inexact-milliseconds)
               #:bind [it]
               it))

   (define old-ms
     (for/first ([ms ms-seq])
       ms))

   (let ([seq
          (in-match '(1 2 3)
                    #:bind [a b c]
                    (list a b c))])
     (check-in-match
      (for/list ([(x y z) seq])
        (vector x y z))
      "Use indirectly in for-clause")
     (check-pred in-value*-record?
                 seq
                 "Should satisfy in-value*-record?")
     (check-in-match
      (for/list ([(x y z) (in-value*/generator seq)])
        (vector x y z))
      "Use with in-value*/generator"))

   (check-equal? (for/first ([ms ms-seq])
                   ms)
                 old-ms
                 "val-expr should be evaluated only once")
   
   (test-case
    "syntax errors"

    (check-exn
     #rx"expected the literal #:bind"
     (λ ()
       (convert-syntax-error
        (in-match '(1 2 3)
                  (list a b c))))
     "#:bind should be required outside for-clause")
    (check-exn
     #rx"^z: unbound identifier in module"
     (λ ()
       (convert-syntax-error
        (in-match '(1 2 3)
                  #:bind [a b z]
                  (list a b c))))
     "should require identifiers bound outside for-clause")
 
    (check-exn
     #rx"^z: unbound identifier in module"
     (λ ()
       (convert-syntax-error
        (for/list ([(a b z) (in-match '(1 2 3)
                                      (list a b c))])
          (vector a b c))))
     "should require identifiers bound inside for-clause")

    (check-exn
     #rx"^z: unbound identifier in module"
     (λ ()
       (convert-syntax-error
        (for/list ([(a b c) (in-match '(1 2 3)
                                      #:bind [a b z]
                                      (list a b c))])
          (vector a b c))))
     "should require identifiers bound inside for-clause with #:bind")

    (check-exn
     #rx"wrong number of results"
     (λ ()
       (convert-syntax-error
        (for/list ([(a b c) (in-match '(1 2 3)
                                      #:bind [a b]
                                      (list a b c))])
          (vector a b c))))
     "in for-clause, should check #:bind has correct number of results"))
   #|END test-case in-match|#))



