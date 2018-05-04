#lang racket/base

(require "require-provide.rkt"
         racket/promise
         racket/contract
         (for-syntax racket/base
                     syntax/parse
                     ))

(require-provide (provide-only "require-provide.rkt")
                 "kernel.rkt"
                 "define-star.rkt"
                 "structure.rkt"
                 "check-args.rkt"
                 ;"find-executable-path.rkt"
                 "link-change-evt.rkt"
                 "in-value-star.rkt"
                 "rx.rkt"
                 "misc.rkt"
                 )

(module+ test
  (require rackunit
           racket/port
           ))

(provide delay/thread/eager-errors
         )


(define-syntax delay/thread/eager-errors
  (syntax-parser
    [(_ (~alt (~optional (~seq #:pred (~var pred (expr/c #'(-> any/c any/c)
                                                         #:name "predicate expression")))
                         #:defaults ([pred.c #'exn:fail?]))
              (~optional (~seq #:handler (~var handler (expr/c #'(-> any/c any)
                                                               #:name "handler expression")))
                         #:defaults ([handler.c #'raise])))
        ...
        body:expr ...+)
     #`(let ([predicate? pred.c]
             [handle handler.c]
             [error-chanel (make-channel)])
         (define error-thread
           (thread
            (λ ()
              (handle (channel-get error-chanel)))))
         (delay/thread
          (with-handlers ([predicate? (λ (e)
                                        (channel-put error-chanel e)
                                        (raise e))])
            body ...)))]))

(module+ test
  (check-equal? (force (delay/thread/eager-errors
                        42))
                42)
  (check-exn #rx"check-delay/thread/eager-errors"
             (λ ()
               (define pr
                 (parameterize ([current-error-port (open-output-nowhere)])
                   (delay/thread/eager-errors
                    (raise
                     (exn:fail "check-delay/thread/eager-errors"
                               (current-continuation-marks))))))
               (force pr)))
  (check-exn exn:break?
             (λ ()
               (let ([th (current-thread)])
                 (delay/thread/eager-errors
                  #:handler (λ (e) (break-thread th))
                  (raise (exn:fail "example" (current-continuation-marks)))))
               (sleep 1))))

