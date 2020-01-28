#lang scribble/manual

@title[#:version ""]{Unstable}
@defmodule[adjutor/unstable]

@(require "utils.rkt"
          scribble/html-properties
          (for-label (except-in racket
                                #%top-interaction)
                     (submod adjutor/unstable/test support)
                     racket/enter
                     adjutor
                     adjutor/unstable
                     syntax/parse
                     racket/require))

Unlike the preceding, features documented in this section are experimental
and/or under development and are subject to breaking changes without notice.

I suggest that before using these features in production code
you check with me about their status or, in the worst-case scenario,
fork the library.

@history[#:changed "0.3"
         @elem{Moved experimental things to @racketmodname[adjutor/unstable]
          to further clarify intent.
          Removed @racketid[struct/derived] in anticipation of
          Racket 7.6 (which will provide a @racket[struct/derived] from
          @racket[racket/base]) and removed the ill-considered
          @racketid[delay/thread/eager-errors].
          Declared @racket[environment-variables-set*] stable.
          }]

@include-section["unstable/link-change-evt.scrbl"]

@section{Miscellaneous Utilities}

@defform[(in-match val-expr maybe-bind-clause pat ...+)
         #:grammar ([maybe-bind-clause
                     (code:line)
                     (code:line #:bind [rslt-id ...])])]{
 A rather unusual form of sequence syntax.
 Using @racket[in-match] creates a single-element, potentially-multi-valued
 sequence, somewhat like @racket[in-value*/expression], but its peculiar
 characteristics make it better thought of as a way of turning a @racket[for]-clause
 into a binding form like @racket[match-let-values] or @racket[match-define].

 The @racket[maybe-bind-clause] is mandatory unless @racket[in-match]
 is used directly within a @racket[for]-clause.

 The @racket[val-expr] is an expression, and each @racket[pat] is a pattern
 for @racket[match]: these are tried against the value of @racket[val-expr]
 in the usual way.
 Each @racket[pat] must bind every @racket[rslt-id],
 or an unbound identifier error will occur.
 The values of the sequence are those bound to the @racket[rslt-id]s (in order)
 by the first @racket[pat] which matches successfully.

 Conceptually, the following sequences written using @racket[in-match]
 and @racket[in-value*/expression] are equivalent:
 @racketblock[
 (in-match val-expr #:bind [rslt-id ...] pat ...+)
 (let ([val val-expr])
   (in-value*/expression
    (match val
      [pat
       (values rslt-id ...)]
      ...+)))]

 Asside from brevity, the key advantage of @racket[in-match] is that
 it installs the values of the @racket[rslt-id]s based on their names,
 eliminating the requirement of getting them in the right order
 in every @racket[match] clause, as one must with @racket[in-value*/expression].

 As a special case, when @racket[in-match] is used directly within a
 @racket[for]-clause, the @racket[maybe-bind-clause] may be omitted.
 Omitting the @racket[maybe-bind-clause] is equivalent to using the same
 identifiers bound by the @racket[for]-clause, so that the following are equivalent:
 @racketblock[
 (for ([(rslt-id ...) (in-match val-expr
                                pat ...+)])
   for-body ...+)
 (for ([(rslt-id ...) (in-match val-expr
                                #:bind [rslt-id ...]
                                pat ...+)])
   for-body ...+)]
 
 If the @racket[maybe-bind-clause] is used inside a @racket[for]-clause,
 it must bind the same number of values expected by the @racket[for]-clause;
 otherwise, a syntax error is raised.

 An @racket[in-match] application can provide better performance
 when it appears directly in a @racket[for]-clause
 (with or without a @racket[maybe-bind-clause]).
 
 @examples[#:eval (make-adjutor/unstable-eval)
           (for*/list ([spec `([3 4 5]
                               [10 20])]
                       [(a b c) (in-match spec
                                          (list a b c)
                                          (list a (and b c)))])
             (+ a b c))
           (for/list ([(x y) (in-match '(1 2)
                                       #:bind [a b]
                                       (list a b))])
             (+ x y))
           (eval:error
            (for/first ([(x y) (in-match '(1 2)
                                         (list x z))])
              (+ x y)))]
}

@defform[(multi subs ...+)
         #:grammar ([subs sub-path (sub-path ...)]
                    [sub-path rel-string id])]{
 Like @racket[multi-in], but for use with @racket[require-provide].

 @history[#:added "0.1"
          ]
}







@section{Static Argument Checking}

@defform[(define/check-args function-header body ...+)]{
 Like the function form of @racket[define], but actually defines a macro that
 statically checks the number (and keywords) of arguments before expanding to
 an application of the underlying function. The @racket[function-header]
 uses the same syntax as @racket[define] (including curried functions, rest arguments,
 etc.), except that a plain identifier is dissalowed, as @racket[define/check-args]
 must be able to determine the required arguments at compile time.

 The resulting function can still be used as a first-class value, but checking
 only occurs for statically visible uses.

 @examples[#:eval (make-adjutor/unstable-eval)
           (eval:error
            (define/check-args (recur arg)
              (cond
                [(pair? arg)
                 (println (car arg))
                 (code:comment "Oops! Forgot the arguments ...")
                 (recur)] 
                [else
                 arg])))]
}

@defform[(define/check-args/contract function-header contract-expr body ...+)
         #:contracts ([contract-expr contract?])]{
 Like @racket[define/check-args], but the resulting function is additionally
 protected by the contract @racket[contract-expr].
 Unlike @racket[define/contract], blame is assigned to the module where
 the function is used (not necessarily the module where it is defined),
 facilitating the export of the identifier bound by @racket[define/check-args/contract].
}







@section{Structures}

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

 @examples[#:eval (make-adjutor/unstable-eval)
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





@include-section["unstable/find-executable-path.scrbl"]
@include-section["unstable/todo.scrbl"]





@section{Testing Meta-Language}
@defmodule[adjutor/unstable/test #:lang #:no-declare]
@declare-exporting[(submod adjutor/unstable/test support)]

The @racketmodname[adjutor/unstable/test] meta-language is useful for
files that should only contain a @racket[test] submodule.
It chains to the following language like @racketmodname[at-exp],
then transforms the result of the other reader to place the
body in a @racket[test] submodule, leaving the enclosing
module empty. 
It also arranges for a special @racket[#%top-interaction] for
the enclosing module so that the REPL is inside the 
@racket[test] submodule.

To a first approximation, this is how a module using
@(racket #,(hash-lang) #,(racketmodname adjutor/unstable/test))
is read compared to some host language:
@(define padding-attribute
   (attributes '([style . "padding:0.5em;"])))
@(tabular
  #:column-properties '((right vcenter right-border) right-border ())
  #:row-properties `((bottom-border ,padding-attribute)
                     (bottom-border ,padding-attribute)
                     ,padding-attribute)
  #:cell-properties `([() center center]
                      [() (left top) (left top)]
                      [() (left top) (left top)])
  (list
   (list ""
         (racket #,(hash-lang) #,(racketmodname adjutor/unstable/test))
         "Host Language")
   (list "Source "
         (racketblock0
          #,(hash-lang) #,(racketmodname adjutor/unstable/test) lang-spec
          body ...)
         (racketblock0
          #,(hash-lang) lang-spec
          body ...))
   (list "Parsed "
         (racketblock0
          (module mod-name #,(racketmodname racket/base)
            (module* test mod-lang
              body ...)))
         (racketblock0
          (module mod-name mod-lang
            body ...)))))

@margin-note{
 The main differences between the above table and the actual
 implementation of @(racket #,(hash-lang) #,(racketmodname adjutor/unstable/test))
 are that a private module language is used instead of
 @racket[racket/base] (to provide a useful @racket[#%top-interaction])
 and that a @racket[configure-runtime] submodule is added.
}

@defform[(#%top-interaction . form)]{
 The @racket[#%top-interaction] installed for the enclosing module
 by @(racket #,(hash-lang) #,(racketmodname adjutor/unstable/test))
 is configured to always @racket[enter!] the @racket[test]
 submodule before evaluating @racket[form].
}





@section{Extending @racket[require-provide]}

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