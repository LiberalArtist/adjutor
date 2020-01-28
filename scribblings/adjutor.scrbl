#lang scribble/manual

@title{Adjutor: A Helper Library}
@author[(author+email @elem{Philip M@superscript{c}Grath}
                      "philip@philipmcgrath.com")]
@defmodule[adjutor]

@(require "utils.rkt"
          (for-label racket
                     adjutor
                     syntax/parse))

Adjutor is a library of (mostly) small, useful utilities
that I have found myself wanting in many different projects.

Most of this library should be regarded as stable, but those portions
documented under @secref["Unstable"]
are, as one might guess, experimental and/or under development.

Bug reports, suggestions, and pull requests are welcome via email
or on @hyperlink["https://github.com/LiberalArtist/adjutor"]{GitHub}.

@(table-of-contents)

@include-section["stable.scrbl"]

@include-section["unstable.scrbl"]









