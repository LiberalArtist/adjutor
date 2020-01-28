#lang racket/base

(require racket/serialize
         racket/match
         racket/contract)

(module+ test
  (require rackunit
           (submod "..")))

(provide (contract-out
          [serialize-to-string
           (-> serializable? (and/c string? immutable?))]
          [deserialize-from-string
           (-> string? any/c)]
          ))

(define (serialize-to-string v)
  (with-handlers
      ([non-serializable-part-error?
        (λ (e)
          (struct-copy exn:fail:contract
                       e
                       [message
                        #:parent exn
                        (regexp-replace #rx"^serialize"
                                        (exn-message e)
                                        "serialize-to-string")]))])
    (string->immutable-string
     (format "~s" (serialize v)))))

(define (deserialize-from-string str)
  (deserialize (read (open-input-string str))))

(module+ test
  (define-check (check-to/from-string v)
    (define deserialized
      (with-handlers ([exn:fail? (λ (e)
                                   (fail-check (exn-message e)))])
        (deserialize-from-string
         (serialize-to-string v))))
    (unless (equal? v deserialized)
      (with-check-info*
       (list (make-check-expected v)
             (make-check-actual deserialized))
       (λ () (fail-check "v did not deserialize to itself")))))

  (check-to/from-string null)
  (check-to/from-string '(1 2 3 "hi" #(O happy "day!")))
  (check-to/from-string (random))
  (serializable-struct example-struct (v)
    #:transparent)
  (check-to/from-string (example-struct '(inner value)))
  #|END module+ test|#)

(define non-serializable-part-error?
  (match-lambda
    [(exn:fail:contract
      (regexp #rx"^serialize: contract violation\n  expected: serializable\\?")
      _)
     #t]
    [_ #f]))
