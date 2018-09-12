#lang scribble/manual

@title[#:version ""]{Extensions to @racket[find-executable-path]}
@defmodule[adjutor/find-executable-path #:no-declare]
@(declare-exporting adjutor/find-executable-path adjutor)

@(require (for-label racket adjutor))

Racket provides @racket[find-executable-path] for finding the paths
of executables (or related files/directories) based on the @tt{PATH}
environment variable.
However, in some cases programmers do not want to use the
system-provided @tt{PATH}.

In particular, on Mac OS, GUI programs are initialized
with a very minimal @tt{PATH}
(at the time of writing, @tt{/usr/bin:/bin:/usr/sbin:/sbin}),
which prevents such programs from finding paths to most user-installed
executables using @racket[find-executable-path] unless the user takes special
measures.

This library provides @racket[find-executable-path*] and related bindings
to address these cases at a higher level than manipulating
@tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{
 environment variable sets} directly.

@defproc[(find-executable-path* [program path-string?]
                                [related (or/c path-string? #f) #f]
                                [deepest? any/c #f]
                                [#:search search-directories (listof path?)
                                 (current-executable-search-directories)])
         (or/c path? #f)]{
 Like @racket[find-executable-path], but searches based on
 @racket[search-directories] rather than the value of
 @racket[(environment-variables-ref (current-environment-variables) #"PATH")].
}

@defparam[current-executable-search-directories search-directories
          (listof path?)]{
 Specifies the list of directories to be searched
 by @racket[find-executable-path*] when no @racket[#:search]
 argument is provided.
 The default value is platform-specific.

 On Mac OS, the default value is obtained by invoking
 Bash as a login shell, printing the @tt{PATH}, and parsing the output.
 This allows the @tt{PATH} to be modified by @filepath{~/.bash_profile},
 @filepath{/etc/paths}, @filepath{/etc/paths.d/}, @etc
 and is consistent with the behavior of @filepath{Terminal.app}.

 On all other platforms, the default value is currently obtained
 by parsing the value of
 @racket[(environment-variables-ref (current-environment-variables) #"PATH")]
 when @racketmodname[adjutor/find-executable-path] is instantiated.

 In the future, alternative ways of computing the default
 value may be supported.
}
           
