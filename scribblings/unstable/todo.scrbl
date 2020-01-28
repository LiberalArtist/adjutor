#lang scribble/manual

@title{Development To-Do Expressions}

@(require adjutor
          (for-label adjutor
                     adjutor/unstable))

@defform*[[(TODO message)
           (TODO #:expr runtime-expr message)
           (TODO message #:expr runtime-expr)]
          #:grammar [(message (code:line msg-datum ...+ maybe-detail))
                     (maybe-detail (code:line)
                                   (code:line #: msg-datum ...+))]]{
 The @racket[TODO] form is intended to be used as a placeholder during
 development. When a @racket[runtime-expr] is given, the entier
 form is equivalent to the @racket[runtime-expr] being used directly.
 If there is no @racket[runtime-expr], evaluating the @racket[TODO] form
 at runtime raises an error (based on the @racket[message]).

 If the @;other-doc['(lib "todo-list/scribblings/todo-list.scrbl")]
 @hyperlink["https://pkgs.racket-lang.org/package/todo-list"]{todo-list}
 plugin is installed (via the @tt{todo-list} package),
 DrRacket will also highlight the placeholders specially.

 A @racket[msg-datum] is implicitly quoted and must me an literal
 string, symbol, or number. These are converted to strings
 with a @racket[" "] added between them to form the message.
 If a @racket[maybe-detail] part is given, it is omited
 from the summary view, for example.

 @(history #:added "0.2.3")
}


@defform[(TODO/void message)]{
 Cooperates with DrRacket like @racket[TODO], but evaluates to
 @void-const at runtime.

 @(history #:added "0.2.3")
}


@defform[(TODO/scrbl [message] runtime-expr ...)]{
 Cooperates with DrRacket like @racket[TODO],
 but designed when for use with the Scribble reader.
 Evaluates to @racket[(begin runtime-expr ...)]
 at runtime.

 @(history #:added "0.2.3")
}




