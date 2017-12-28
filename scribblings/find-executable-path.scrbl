#lang scribble/manual

@title[#:version ""]{Extensions to @racket[find-executable-path]}
@defmodule[adjutor/find-executable-path]

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
                                [deepest? any/c #f])
         (or/c path? #f)]{
 Like @racket[find-executable-path], but uses a @tt{PATH} based on
 @racket[current-preferred-PATH-spec] rather than
 @racket[current-environment-variables].
}

@deftogether[(@defparam[current-preferred-PATH-spec spec
                        preferred-PATH-spec/c]
               @defthing[preferred-PATH-spec/c contract?
                         #:value
                         (or/c 'inherit
                               #f 
                               bytes-no-nuls? 
                               string-no-nuls?)])]{
 The parameter @racket[current-preferred-PATH-spec] controls the
 @tt{PATH} used by @racket[find-executable-path*].
 On platforms other than Mac OS, the default value is @racket['inherit].

 On Mac OS, the default value is a byte string obtained by invoking
 Bash as a login shell and printing the @tt{PATH}.
 This allows the @tt{PATH} to be modified by the user's @tt{.bash_profile}
 and is consistent with the behavior of @tt{Terminal.app}.

 The derived parameter @racket[current-preferred-PATH] can be used to
 access the actual byte string which @racket[find-executable-path*] will
 use as the @tt{PATH}.

 The meaning of @racket[spec] is as follows:
 @itemlist[@item{A value of @racket['inherit] indicates that the @tt{PATH} from
             @racket[current-environment-variables] should be used, in
             which case @racket[find-executable-path*] will work just like
             @racket[find-executable-path].
            }
           @item{A value of @racket[#f] means to use an environment with
             no mapping for @tt{PATH}.
            }
           @item{A byte string satisfying @racket[bytes-no-nuls?] is
             coerced to an immutable byte string and used as the @tt{PATH}.
            }
           @item{A string satisfying @racket[string-no-nuls?] is
             coerced to an immutable string, and the @tt{PATH}
             is the result of converting the string to an (immutable)
             byte string using @racket[string->bytes/locale].
             }]

 Support for additional kinds of @racket[spec] is planned for the future,
 in which case the @racket[preferred-PATH-spec/c] contract will be extended.
}

@defparam*[current-preferred-PATH spec
           preferred-PATH-spec/c (or/c #f (and/c bytes-no-nuls?
                                                 immutable?))]{
 An alternative interface to the same parameter as @racket[current-preferred-PATH-spec]
 (see @racket[make-derived-parameter]), but accesses the actual
 byte string which @racket[find-executable-path*] will
 use as the @tt{PATH} (or @racket[#f] if it will use an environment with
 no mapping for @tt{PATH}).
}
           
