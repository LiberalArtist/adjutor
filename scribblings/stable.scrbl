#lang scribble/manual

@title{Stable}

@(require "utils.rkt"
          (for-label racket
                     adjutor
                     syntax/parse
                     racket/serialize
                     ))

This section documents the stable portion of @racketmodname[adjutor].

@section{Iteration Forms}

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

 Note that the @racket[#:result] clause of @racket[for/fold] and @racket[for*/fold]
 (added in Racket 6.11.0.1) is not currently supported.
 
 @examples[#:eval (make-adjutor-eval)
           (for/fold/define ([keys '()]
                             [vals '()])
                            ([pr '([a . 1]
                                   [b . 2]
                                   [c . 3])])
             (match pr
               [(cons k v)
                (values (cons k keys)
                        (cons v vals))]))
           keys
           vals]
}

@deftogether[(@defform[(for/lists/define (id ...)
                                         (for-clause ...)
                         body-or-break ... body)]
               @defform[(for*/lists/define (id ...)
                                           (for-clause ...)
                          body-or-break ... body)])]{
 Similar to @racket[for/lists] and @racket[for*/lists], respectively,
 but binds each @racket[id] to the corresponding final result
 of the iterations similarly to
 @racket[for/fold/define] and @racket[for*/fold/define].
 
 @examples[#:eval (make-adjutor-eval)
           (for/lists/define (keys vals)
                             ([pr '([a . 1]
                                    [b . 2]
                                    [c . 3])])
             (match pr
               [(cons k v)
                (values k v)]))
           keys
           vals]
}

@section{Guarded Evaluation: @racket[when]-like Forms}

@deftogether[(@defform[(string-when test body ...+)]
               @defform[(string-unless test body ...+)])]{
 Like @racket[when] and @racket[unless], respectively,
 but with @racket[""] (rather than
 @void-const) as the default value when @racket[test] 
 prevents the evaluation of the @racket[body] forms.
 @examples[#:eval (make-adjutor-eval)
           (string-when (= 1 1)
             "This is returned")
           (string-unless (= 1 1)
             "Default is returned")
           (string-when #f
             (+ 42 "This would be an error"))]
}

@deftogether[(@defform[(list-when test body ...+)]
               @defform[(list-unless test body ...+)])]{
 Like @racket[string-when] and @racket[string-unless],
 respectively, but with @racket[null] as the default value.
 @examples[#:eval (make-adjutor-eval)
           (list-when (= 1 1)
             `(1 1 2 3 5 8))
           (list-unless (= 1 1)
             "Default is returned")
           (list-when #f
             (+ 42 "This would be an error"))]
}


@section{Small Utilities}


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

@defproc[(any->boolean [x any/c])
         boolean?]{
 Returns @racket[#t] for any input but @racket[#f].
}

@defform[(infix: left op right)]{
 Provides infix notation for binary operators, expanding to
 @racket[(op left right)].

 Infix notation is often useful, especially for noncommutative
 binary operators like @racket[<].
 Racket supports reader-based infix notation by default,
 so you can write @racket[(1 . < . 2)].
 However, a disadvantage of this notation for those used to reading
 Racket syntax is that the beginning of the s-expression doesn't
 make it obvious that infix notation is being used.
 This is particularly a problem when the expression immediately
 following the @litchar{(} is more complex than @racket[1], as
 it might be confused with a function or macro application.

 This macro provides an alternative infix notation specialized for
 the common case of binary operators, allowing the above expression
 to be written as @racket[(infix: 1 < 2)].
}


@defthing[ip-port-num/c flat-contract?
          #:value (integer-in 0 65535)]{
 A contract recognizing legal IP port numbers.

 @(history
   #:added "0.2.1"
   )}


@section{Regular Expressions}


@deftogether[(@defproc*[([(rx [#:handler handler (or/c #f (-> any/c any)) #f]
                              [arg string?] ...)
                          regexp?]
                         [(rx [#:handler handler (or/c #f (-> any/c any)) #f]
                              [arg bytes?] ...+) byte-regexp?])]
               @defproc*[([(px [#:handler handler (or/c #f (-> any/c any)) #f]
                               [arg string?] ...) pregexp?]
                          [(px [#:handler handler (or/c #f (-> any/c any)) #f]
                               [arg bytes?] ...+) byte-pregexp?])])]{
 Functions that concatenate their arguments into a regular expression value.
 A @racket[handler], if given, used as with @racket[regexp] etc.
 
 As a special case, in an application visible at compile-time
 when every @racket[arg] is a literal string or byte string, the regular
 expression value is generated at compile time rather than runtime.
 In this case, any @racket[handler] expression is not even evaluated:
 instead, a compile-time error is raised if the arguments are invalid.

 These functions are particularly designed for use with the
 @secref["reader" #:doc '(lib "scribblings/scribble/scribble.scrbl")].
}

@section{Definitions}

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

@defform[(define-alias new-id orig-id)]{
 Short for @racket[(define-syntax new-id (make-rename-transformer #'orig-id))]
}

@defform[(define-aliases [new-id orig-id] ...+)]{
 Short for multiple @racket[define-alias] forms.
}

@section{Serialization}
@deftogether[(@defproc[(serialize-to-string [v serializable?])
                       (and/c string? immutable?)]
               @defproc[(deserialize-from-string [str string?])
                        any/c])]{
 Like @racket[serialize] and @racket[deserialize], respectively,
 but using a string for the serialized value. 
                                 
 @history[#:added "0.2.5"]
}

@section{Sequence Constructors}

In addition to the stable sequence constructors documented in this
section, the experimental portion of @racket[adjutor] provides @racket[in-match].

@(define-syntax-rule (performance-note who)
   @list{An @racket[who] application can provide better performance
 when it appears directly in a @racket[for] clause.})

@defproc[(in-value* [expr any/c] ...)
         (and/c sequence? in-value*-record?)]{
 Produces a sequence which is similar to those produced by @racket[in-value]
 in that it has only a single element; however, whereas @racket[in-value]
 produces single-valued sequences, the sequence produced by @racket[in-value*]
 has as many values as there are @racket[expr] expressions.

 In other words, if @racket[in-value] is "useful for @racket[let]-like bindings in forms
 such as @racket[for*/list]", @racket[in-value*] is useful for
 @racket[let-values]-like bindings.

 @(performance-note in-value*)

 @examples[#:eval (make-adjutor-eval)
           (for/list ([(a b c) (in-value* 1 2 3)])
             (vector a b c))
           (define seq
             (in-value*))
           seq
           (for/first ([() seq])
             '|This works|)]
                       
}

@deftogether[(@defproc[(in-value*/generator [generator (or/c (-> any) in-value*-record?)])
                       (and/c sequence? in-value*-record?)]
               @defproc[(in-value*-record? [v any/c]) any/c])]{
 The function @racket[in-value*/generator] creates a single-element,
 potentially-multi-valued sequence like @racket[in-value*],
 where the values are determined by @racket[generator].
 If @racket[generator] is a thunk, the values are the results of calling @racket[generator].
 Otherwise, @racket[generator] must be a sequence constructed using
 @racket[in-value*], @racket[in-value*/expression], or @racket[in-value*/generator],
 in which case @racket[generator] is returned directly (but potentially with
 a performance advantage when used directly in a @racket[for] clause).
 Sequences that may be used for @racket[generator] are recognized by the predicate
 @racket[in-value*-record?].
 
 @(performance-note in-value*/generator) 

 @examples[#:eval (make-adjutor-eval)
           (define (gen)
             (values "apples" "peaches" "pears"))
           (for/list ([(a b c) (in-value*/generator gen)])
             (string-append a " & " b " & " c))
           (define seq
             (in-value* "apples" "peaches" "pears"))
           (for/list ([(a b c) (in-value*/generator seq)])
             (string-append a " & " b " & " c))]
}

@defform[(in-value*/expression body-expr)]{
 Creates a sequence that is conceptually equivalent to
 @racket[(in-value*/generator (λ () body-expr))].
 Note that this means that @racket[body-expr] is evaluated each time
 the resulting sequence is 
 @tech[#:key "initiate" #:doc '(lib "scribblings/reference/reference.scrbl")]{
  initiated}.
 
 @(performance-note in-value*/expression) Using @racket[in-value*/expression]
 may also have advantages over @racket[in-value*/generator] in such cases.

 @examples[#:eval (make-adjutor-eval)
           (define seq
             (in-value*/expression
              (current-inexact-milliseconds)))
           (for/first ([msec seq])
             msec)
           (for*/list ([task `([1 1 2]
                               [4 5]
                               [5 5 3]
                               [15 35 8])]
                       [(a b c) (in-value*/expression
                                 (match task
                                   [(list a b c)
                                    (values a b c)]
                                   [(list a b)
                                    (values a b 0)]))])
             (- (+ a b) c))
           (for/first ([msec seq])
             msec)]
}



@section{@racket[require-provide]: Abbreviations for Re-Exporting}

@defform[#:literals (only-in except-in prefix-in rename-in 
                             only-meta-in except-out rename-out prefix-out
                             for-syntax for-template for-label for-meta
                             provide-only)
         (require-provide require-provide-spec ...)
         #:grammar ([require-provide-spec
                     module-path
                     adjust-require
                     adjust-provide
                     derived-require-provide-spec]
                    [adjust-require
                     (only-in require-provide-spec id-maybe-renamed ...)
                     (except-in require-provide-spec excluded-id ...)
                     (prefix-in prefix require-provide-spec)
                     (rename-in require-provide-spec
                                [orig-id in-id] ...)
                     (only-meta-in phase-level require-provide-spec)
                     (provide-only module-path ...)]
                    [adjust-provide
                     (except-out require-provide-spec excluded-id ...)
                     (rename-out require-provide-spec
                                 [orig-id out-id] ...)
                     (prefix-out prefix require-provide-spec)]
                    [adjust-both
                     (for-syntax require-provide-spec ...)
                     (for-template require-provide-spec ...)
                     (for-label require-provide-spec ...)
                     (for-meta phase-level require-provide-spec ...)]
                    [id-maybe-renamed orig-id [orig-id renamed]])]{
 In the simplest case, when @racket[require-provide-spec] is
 a plain @racket[module-path] (as specified by @racket[require]
 and @racket[provide]) for
 @(racketblock
   (require module-path ...)
   (provide (all-from-out module-path ...)))

 Other kinds of @racket[require-provide-spec] adjust what is
 @racket[require]d, @racket[provide]d, or both. Note that a few have a
 subtly different grammar than when used with @racket[require] or @racket[provide].
 All are recognized by binding, rather than symbolically.

 For details on @racket[derived-require-provide-spec] (i.e. programmer-implemented
 extensions to @racket[require-provide]), see @secref["Extending_require-provide"]
 below, but note that the extension mechanism is unstable and subject to change.
 
 @examples[#:eval (make-adjutor-eval) @;"provide: not at module level"
           (module example racket/base
             (require adjutor)
             (require-provide (provide-only adjutor)
                              (prefix-in : racket/stream)
                              (prefix-out : racket/string))
             (:stream 1 2 3)
             (string-join '("Works without a prefix here,"
                            "but exported with a prefix.")
                          " "))
           (require 'example)
           (string-when #t
             (:string-join '("Here," "it has a prefix")
                           " "))
           (:stream "Still has a prefix")]
}

@defform[(provide-only module-path ...)]{
 The @racket[provide-only] form cooperates with @racket[require-provide]
 to export all bindings from each @racket[module-path], which should have been
 used in a @racket[require] form elsewhere.

 This might be desireable when a module is re-exporting bindings from a number of
 other modules via @racket[require-provide], but also wants to export bindings from
 some module which it had to import via @racket[require], perhaps because that
 module gave it the @racket[require-provide] binding itself.
 Thus, for example,
 @racketblock[(require adjutor)
              (require-provide (provide-only adjutor))]
 could be used as an alternative to
 @racketblock[(require adjutor)
              (provide (all-from-out adjutor))]

 Use of @racket[provide-only] outside of a @racket[require-provide] form
 is a syntax error.
}
