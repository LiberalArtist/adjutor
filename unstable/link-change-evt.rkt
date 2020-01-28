#lang racket/base

(require racket/contract
         )

(provide filesystem/link-change-evt?
         (contract-out
          [filesystem/link-change-evt-cancel
           (-> filesystem/link-change-evt? any)]
          [rename filesystem/link-change-evt*
                  filesystem/link-change-evt
                  (-> path-string?
                      filesystem/link-change-evt?)]
          ))

(struct filesystem/link-change-evt (cancel-thunk evt)
  #:property prop:evt (struct-field-index evt))

(define (filesystem/link-change-evt-cancel it)
  ((filesystem/link-change-evt-cancel-thunk it)))

(define (filesystem/link-change-evt* raw-pth)
  (define pth
    (path->complete-path raw-pth))
  (cond
    [(link-exists? pth)
     (make-link-evt pth)]
    [else
     (define cust
       (make-custodian))
     (filesystem/link-change-evt
      (λ () (custodian-shutdown-all cust))
      (parameterize ([current-custodian cust])
        (filesystem-change-evt pth)))]))

(define (make-link-evt pth)
  (define cust
    (make-custodian))
  (define-values {dir name must-be-dir?}
    (split-path pth))
  (define orig-target-id
    (file-or-directory-identity pth))
  (define (make-dir-changed-evt)
    (if (custodian-shut-down? cust)
        never-evt
        (parameterize ([current-custodian cust])
          (filesystem-change-evt dir))))
  (define bx:latest-dir-changed-evt
    (box (make-dir-changed-evt)))
  (define (cancel-thunk)
    (set-box! bx:latest-dir-changed-evt never-evt)
    (custodian-shutdown-all cust))
  (define (wrap-with-cleanup evt)
    (wrap-evt evt
              (λ (_) (cancel-thunk))))
  (define orig-target-changed-evt
    (wrap-with-cleanup
     (parameterize ([current-custodian cust])
                (filesystem-change-evt pth))))
  (define (on-check-dir-evt _)
    (cond
         [(and (link-exists? pth)
               (= orig-target-id
                  (file-or-directory-identity pth)))
          (set-box! bx:latest-dir-changed-evt
                    (make-dir-changed-evt))
          (make-check-dir-evt)]
         [else
          (wrap-with-cleanup always-evt)]))
  (define (make-check-dir-evt)
    (replace-evt
     (unbox bx:latest-dir-changed-evt)
     on-check-dir-evt))
  (filesystem/link-change-evt
   cancel-thunk
   (choice-evt orig-target-changed-evt
               (guard-evt make-check-dir-evt)
               (make-custodian-box cust 'live))))


