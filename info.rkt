#lang info

(define collection "adjutor")

(define deps '("base"
               "rackunit-lib" ; why isn't this a build-dep?
               "static-rename-lib"))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-spec"
                     "scribble-doc"))

(define scribblings '(("scribblings/adjutor.scrbl"
                       (multi-page)
                       )))

(define pkg-desc "A helper library")

(define version "0.0")

(define pkg-authors '(philip))
