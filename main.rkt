#lang racket/base

(require "require-provide.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(require-provide (provide-only "require-provide.rkt")
                 "kernel.rkt"
                 "define-star.rkt"
                 "structure.rkt"
                 )

(provide delay/thread/eager-errors
         )

(define-syntax delay/thread/eager-errors
  (syntax-parser
    [(_ body:expr ...+)
     #`(let* ([error-chanel (make-channel)]
              [error-thread (thread
                             (λ ()
                               (raise (channel-get error-chanel))))])
         (delay/thread
          (with-handlers ([exn:fail? (λ (e)
                                       (channel-put error-chanel e)
                                       (raise e))])
            body ...)))]))
