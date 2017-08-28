#lang scribble/manual

@title{Adjutor: A Helper Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com"
                      #:obfuscate? #t)]
@defmodule[adjutor]

@(require scribble/example
          (for-label racket
                     adjutor
                     syntax/parse
                     ))

Adjutor is a library of (mostly) small, useful utilities
that I have found myself wanting in many different projects.

Most of this library should be regarded as stable (I rely on it
as stable in a number of different projects), but those portions
documented under @secref["Experimental"]
are, as one might guess, experimental and/or under development.

Bug reports, suggestions, and pull requests are welcome via email
or on @hyperlink["https://github.com/LiberalArtist/adjutor"]{GitHub}.

@(define make-adjutor-eval
   (make-eval-factory '(adjutor racket/contract racket/match)))

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

@defform[(define-alias new-id orig-id)]{
 Short for @racket[(define-syntax new-id (make-rename-transformer #'orig-id))]
}

@defform[(define-aliases [new-id orig-id] ...+)]{
 Short for multiple @racket[define-alias] forms.
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




@section{Experimental}

Unlike the preceding, features documented in this section are experimental
and/or under development and are subject to breaking changes without notice.

I obviously don't intend to break things gratuitously, but I suggest that before
using these features in production code you check with me about their status
or, in the worst-case scenario, fork the library.

@defform[(delay/thread/eager-errors body ...+)]{
 Like @racket[(delay/thread body ...)], but, if
 forcing the promise would raise an exception
 satisfying @racket[exn:fail?], the exception is also raised
 asynchronously as soon as it is encountered in the background
 thread.
}

@subsection{Structures}

@defform[(struct/derived
          (error-id orig-form ...)
          id maybe-super (field ...)
          struct-option ...)
         #:grammar ([maybe-super (code:line) super-id])]{
 Defines a new structure type like @racket[struct], but
 with syntax errors reported in terms of @racket[(error-id orig-form ...)]
 like @racket[define-struct/derived].

 See @racket[struct] for the grammar of @racket[field]
 and @racket[struct-option].
}

@defform[(structure id maybe-super (field ...)
           option ...)
         #:grammar ([maybe-super (code:line) super-id]
                    [option structure-option restricted-struct-option]
                    [structure-option
                     (code:line #:constructor-contract contract-expr)
                     (code:line #:constructor constructor-wrapper-expr)
                     (code:line #:match-expander new-match-transformer)])
         #:contracts
         ([contract-expr contract?]
          [constructor-wrapper-expr contract-expr]
          [new-match-transformer
           (code:line (-> syntax? syntax?) (code:comment "in the transformer environment"))])]{
 In the simplest case, when no @racket[structure-option]s are
 given, defines a new structure type like @racket[struct],
 where a @racket[restricted-struct-option] is any option that can be given to
 @racket[struct] except for @racket[#:constructor-name],
 @racket[#:extra-constructor-name], @racket[#:name], and @racket[#:extra-name].

 Any @racket[structure-option]s that are given control the meaning of @racket[id].

 If a @racket[#:constructor] option is given, the @racket[constructor-wrapper-expr]
 is accessed instead of the default constructor when @racket[id] is used as an
 expression. The @racket[constructor-wrapper-expr] must satisfy the
 contract @racket[contract-expr] if a @racket[#:constructor-contract] option is given;
 otherwise, it is required to be a procedure. Inside the @racket[constructor-wrapper-expr],
 @racket[raw-constructor] can be used to access the default constructor.

 If a @racket[#:constructor-contract] option is given without a
 @racket[#:constructor] option, the default constructor is protected with
 the contract @racket[contract-expr].
 
 If a @racket[#:match-expander] clause is given, the @racket[new-match-transformer]
 must be an expression in the transformer environment that produces a function from
 syntax to syntax. It is used instead of the default pattern-matching behavior when
 @racket[id] is used as a
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{match expander}.
 Inside the @racket[new-match-transformer], @racket[raw-match-transformation]
 can be used to implement transformers that expand to the default pattern-matching
 behavior.

 As with @racket[struct], @racket[id] can be used as a structure type transformer
 which can be used to define subtypes and cooperates with @racket[shared],
 @racket[struct-out], etc. (But note that a @racket[#:match-expander] clause
 contols @racket[id]'s cooperation with @racket[match].) For more detailed
 information about these uses of @racket[id], see
 @secref["structinfo" #:doc '(lib "scribblings/reference/reference.scrbl")].
 
 @(nested
   #:style 'inset
   @defidform[raw-constructor]{
  Within a @racket[#:constructor] clause of @racket[structure],
  accesses the plain constructor for the structure type.
  Illegal elsewhere.
 }
   @defidform[raw-match-transformation]{
  Within a @racket[#:match-expander] clause of @racket[structure],
  accesses a function that accepts a syntax object of the shape
  @racket[#'(_ pat ...)] and returns syntax for @racket[match]
  that matches each @racket[pat] against the fields of the structure.
  Illegal elsewhere.
  })

 @examples[#:eval (make-adjutor-eval)
           (require (for-syntax racket/base syntax/parse))
           (structure point (x y z)
             #:transparent
             #:constructor (λ ([x 0] [y 0] [z 0])
                             (raw-constructor x y z))
             #:constructor-contract (->* {}
                                         {real? real? real?}
                                         point?)
             #:match-expander (syntax-parser
                                [(_ x y z)
                                 (raw-match-transformation #'(_ x y z))]
                                [(_ x y)
                                 #'(point x y _)]
                                [(_ x)
                                 #'(point x _ _)]
                                [(_)
                                 #'(point _ _ _)]))
           (match (point)
             [(point x) x])
           (struct-copy point (point 5)
                        [z 42])
           (eval:error (point #f))]
}






@subsection{Extending @racket[require-provide]}

The bindings in this section are provided @racket[for-syntax]
to be used in implementing extensions to @racket[require-provide].
They are particularly experimental and subject to change.

@defstruct*[simple-require-provide-transformer
            ([proc (-> syntax? syntax?)])]{
 A @racket[simple-require-provide-transformer] contains a function @racket[proc]
 (perhaps created @racket[syntax-parser]) that specifies a simple
 macro-like rewrite rule. Like a macro, @racket[proc] is called with the
 parenthesized form beginning with the identifier
 bound to the transformer itself. Its result must be a
 @racket[require-provide-spec].
}

@defstruct*[require-provide-transformer
            ([proc (-> syntax?
                       (values syntax? syntax?))])]{
 A @racket[require-provide-transformer] is like a 
 @racket[simple-require-provide-transformer], but more general.
 Its @racket[proc] returns two syntax objects: the first must be a valid
 @racket[require-spec] for use with @racket[require], and the second must
 be a @racket[provide-spec] for use with @racket[provide].
}

@deftogether[(@defidform[#:kind "syntax class" module-path]
               @defidform[#:kind "syntax class" phase-level])]{
 Syntax classes recognizing the grammer for module paths and phase levels as
 specified by @racket[require] and @racket[provide].
}

@defidform[#:kind "syntax class" require-provide-spec]{
 A syntax class recognizing the grammar documented under @racket[require-provide],
 including @racket[derived-require-provide-spec]s created with
 @racket[require-provide-transformer] or @racket[simple-require-provide-transformer].
 It has two attributes @racket[require-stx] and @racket[provide-stx], which
 are the syntax to be passed on to @racket[require] and @racket[provide], respectively.
}











