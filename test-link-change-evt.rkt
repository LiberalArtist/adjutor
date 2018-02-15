#lang racket/base

(module+ test
  (require rackunit
           racket/file
           "link-change-evt.rkt"
           ))

;; Tested on Mac OS and Ubuntu.
;; n.b. Windows only has directory-level precision.

(module+ test
  (define basedir
    (make-temporary-file "rkttmp~a" 'directory))

  (define (make-parent-directories . args)
    (for ([pth (in-list args)])
      (make-parent-directory* pth)))

  (define a-path
    (build-path basedir "a" "a"))

  (define b-path
    (build-path basedir "b" "b"))

  (define c-path
    (build-path basedir "c" "c"))

  (define link-path
    (build-path basedir "link" "link"))

  (define (main)
    (make-parent-directories a-path
                             b-path
                             c-path
                             link-path)
    (write-to-file 'a a-path)
    (write-to-file 'b b-path)
    (make-file-or-directory-link a-path link-path)
    (define evt
      (filesystem/link-change-evt link-path))
    (let/ec return
      (define (check-evt message)
        (sync/timeout 0
                      (handle-evt evt
                                  (λ (_)
                                    (return message)))))
      (dynamic-wind
       void
       (λ ()
         (check-evt "At start")
         (write-to-file 'c c-path)
         (check-evt "Sibling file changed")
         (delete-file link-path)
         (check-evt "Link deleted")
         (make-file-or-directory-link b-path link-path)
         (check-evt "New link created")
         (write-to-file 'changed b-path #:exists 'truncate/replace)
         (check-evt "New target changed")
         (write-to-file 'changed a-path #:exists 'truncate/replace)
         (check-evt "Orig target changed")
         (return "Shouldn't get here"))
       (λ ()
         (filesystem/link-change-evt-cancel evt)))))

  (check-equal? (dynamic-wind
                 void
                 main
                 (λ ()
                   (delete-directory/files
                    basedir
                    #:must-exist? #f)))
                "Link deleted"
                "Test of filesystem/link-change-evt")
  #|END module+ test|#)
