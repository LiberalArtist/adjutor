#lang info

(define pkg-name "adjutor")
(define collection "adjutor")
(define pkg-desc "A helper library")
(define version "0.3.2")
(define pkg-authors '(philip))

;; Dependencies:
(define deps '(("base" #:version "6.12")
               "static-rename-lib"))

(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "rackunit-spec"
                     "scribble-doc"))

;; Documentation:
(define scribblings '(("scribblings/adjutor.scrbl"
                       (multi-page)
                       )))


