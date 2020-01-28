#lang racket/base

(require adjutor)

(require-provide scribble/example)

(provide make-adjutor-eval
         make-adjutor/unstable-eval)

(define libs
  '(adjutor racket/contract racket/match))

(define make-adjutor-eval
   (make-eval-factory libs))

(define make-adjutor/unstable-eval
  (make-eval-factory (cons 'adjutor/unstable libs)))
