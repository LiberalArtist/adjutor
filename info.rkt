#lang info

(define collection "adjutor")

(define deps '("base"
               "rackunit-lib"))

(define build-deps '("scribble-lib" "racket-doc"))

(define scribblings '(("scribblings/adjutor.scrbl"
                       ()
                       (experimental)
                       )))

(define pkg-desc "A helper library")

(define version "0.0")

(define pkg-authors '(philip))
