#lang racket/base

(require racket/contract
         racket/system
         racket/match
         )

(provide (contract-out
          [environment-variables-set*
           (->* {environment-variables?}
                #:rest environment-variables-args/c
                environment-variables?)]
          ))

(define environment-variables-args/c
  (flat-rec-contract environment-variables-args/c
    (list)
    (cons/c bytes-environment-variable-name?
            (cons/c (or/c bytes-no-nuls? #f)
                    environment-variables-args/c))))

(define (environment-variables-set* env . args)
  (let ([env (environment-variables-copy env)])
    (let loop ([args args])
      (match args
        [(list-rest k v args)
         (environment-variables-set! env k v)
         (loop args)]
        ['()
         env]))))
