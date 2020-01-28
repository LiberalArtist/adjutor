#lang scribble/manual

@title[#:version ""]{Detecting Symbolic Link Changes}
@declare-exporting[adjutor/unstable]


@(require (for-label racket
                     adjutor
                     adjutor/unstable))


@(define (filesystem-change-events)
   @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{filesystem change events})

@deftogether[
 (@defproc[(filesystem/link-change-evt [path-string path-string?])
           filesystem/link-change-evt?]
   @defproc[(filesystem/link-change-evt-cancel [evt filesystem/link-change-evt?])
            any]
   @defproc[(filesystem/link-change-evt? [v any/c]) any/c])]{
 Similar to @racket[filesystem-change-evt], @racket[filesystem-change-evt-cancel],
 and @racket[filesystem-change-evt?], respectively, except that the events
 created by @racket[filesystem/link-change-evt] become ready at additional
 times when @racket[path-string] is a symbolic link
 (according to @racket[link-exists?]).

 Specifically, for symbolic links, @(filesystem-change-events)
 detect changes on the target of the symbolic link, not on the
 link itself.
 In addition to detecting those changes,
 events created by @racket[filesystem/link-change-evt]
 also become ready for synchronization when the link itself is deleted or changed
 to point to a different target
 (according to @racket[file-or-directory-identity]).

 Like @(filesystem-change-events), events created by
 @racket[filesystem/link-change-evt] allocate resources at the
 operating-system level, which are placed in the custody of the
 @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{current custodian}.
 These resources are released automatically when the event
 is chosen for synchronization: otherwise, they
 must be released by shutting down the custodian via
 @racket[custodian-shutdown-all] or by explicitly calling
 @racket[filesystem/link-change-evt-cancel].
 In either case, the event becomes ready for synchronization
 (if it is not already).

 @margin-note{
  Note that these functions remain experimental.
  In particular, the desired behavior on Windows (where filesystem changes
  can be tracked only at directory-level resolution) has not been determined.
 }
 
 @(history
   #:added "0.2"
   )}

