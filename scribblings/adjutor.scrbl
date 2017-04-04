#lang scribble/manual

@title{Adjutor: A Helper Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com"
                      #:obfuscate? #t)]
@defmodule[adjutor]

@(require scribble/example
          (for-label racket
                     adjutor
                     ))

Adjutor is a library of (mostly) small, useful utilities
that I have found myself wanting in many different projects.

While I have no intention of breaking things, this library
should be regarded as experimental in nature.

@(define make-adjutor-eval
   (make-eval-factory '(adjutor)))

@defform[(require-provide module-path ...)]{
 Shorthand for
 @(racketblock
   (require module-path ...)
   (provide (all-from-out module-path ...)))
 @;{Actually it supports a lot more than that, which I should
  document more fully.}
}

@deftogether[(@defform[(for/fold/define ([accum-id init-expr] ...)
                                        (for-clause ...)
                         body-or-break ... body)]
               @defform[(for*/fold/define ([accum-id init-expr] ...)
                                          (for-clause ...)
                          body-or-break ... body)])]{
 Like @racket[for/fold] and @racket[for*/fold], respectively, but rather than
 returning (potentially multiple) values, binds each @racket[accum-id] to the
 final result of the iterations.

 For example, @racket[for/fold/define] is equivalent to
 @(racketblock
   (define-values (accum-id ...)
     (for/fold ([accum-id init-expr] ...)
               (for-clause ...)
       body-or-break ... body)))
}

@defform[(string-when test body ...+)]{
 Like @racket[when], but returns @racket[""] (rather than
 @racket[(void)]) when @racket[test] is @racket[#f].
 The @racket[body]
 expression is only evaluated when @racket[test] passes.
 @examples[#:eval (make-adjutor-eval)
           (string-when (= 1 1)
             "This is returned")
           (string-when #f
             (+ 42 "This would be an error"))]
}

@defform[(list-when test body ...+)]{
 Like @racket[string-when], but returns @racket[null]
 when @racket[test] is @racket[#f].
 @examples[#:eval (make-adjutor-eval)
           (list-when (= 1 1)
             `(1 1 2 3 5 8))
           (list-when #f
             (+ 42 "This would be an error"))]
}

@deftogether[(@defform[(values->list body ...+)]
               @defproc[(list->values [lst list?])
                        any])]{
 Helpers to make forms that expect and/or produce multiple
 return values interoperable with those that produce and
 consume lists.
 @examples[#:eval (make-adjutor-eval)
           (values->list (values 1 2 3 4))
           (define-values (val1 val2)
             (list->values `(first-item second-item)))
           val1
           val2]
}

@defform[(define-alias new-id orig-id)]{
 Short for @racket[(define-syntax new-id (make-rename-transformer #'orig-id))]
}

@defform[(define-aliases [new-id orig-id] ...+)]{
 Short for multiple @racket[define-alias] forms.
}

@defproc[(any->boolean [x any/c])
         boolean?]{
 Returns @racket[#t] for any input but @racket[#f].
}

@defform[(define* define-lhs maybe-with-clause body ...+)
         #:grammar ([maybe-with-clause
                     (code:line)
                     (code:line #:with [definition-or-expr ...])])]{
The function form of @racket[define] helps with readability,
but it doesn't work well for functions that want to maintain
private state via a "let-over-lambda". Also, when
binding a plain identifier with @racket[define], it is a syntax
error to have "multiple expressions after identifier". These
limitations are solved with @racket[define*].

When the @racket[#:with] clause is ommited, @racket[define*] works
just like @racket[define], except that multiple @racket[body]
forms are allways allowed. (As usual, these can be arbitrary
definitions and expressions, but they must end with an expression.)

If a @racket[#:with] clause is given, it may contain arbitrary
definitions and expressions (and may conclude with a definition).
These are lifted outside the function, if applicable, and any
identifiers defined in the @racket[#:with] clause are only in
scope within the @racket[define*] form. If @racket[define-lhs]
specifies a curried function, the @racket[#:with] clause
is evaluated outside of the outermost function.

@examples[#:eval (make-adjutor-eval)
          (define* (add-to-base n)
            #:with [(define base
                      (random 100))]
            (+ n base))
          (add-to-base 5)
          (add-to-base 5)
          (define* watermellons
            (displayln "Defining watermellons!")
            "watermellons")
          watermellons]
}

@defform[(def def-clause ...)
         #:grammar ((def-clause
                      [id rhs]
                      [function-header body ...+]))]{
 Concisely define multiple identifiers with a
 @racket[let]-like syntax that also supports the best features of
 @racket[define].
 @examples[#:eval (make-adjutor-eval)
           (def
             [(add2 n) (+ 2 n)]
             [fruits '(apples peaches pears)])
           (add2 5)
           fruits]
}