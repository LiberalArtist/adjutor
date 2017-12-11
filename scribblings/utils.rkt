#lang racket/base

(require adjutor)

(require-provide scribble/example)

(provide make-adjutor-eval)

(define make-adjutor-eval
   (make-eval-factory '(adjutor racket/contract racket/match)))
