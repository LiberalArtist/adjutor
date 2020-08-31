#lang info

(define pkg-name "adjutor")
(define collection "adjutor")
(define pkg-desc "A helper library")
(define version "0.3.3")
(define pkg-authors '(philip))

;; Documentation:
(define scribblings
  '(("scribblings/adjutor.scrbl" (multi-page))))

;; Dependencies:
(define deps
  '(["base" #:version "7.0"]
    "static-rename-lib"))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"
    "rackunit-spec"
    "scribble-doc"))

