#lang racket/base

(module reader syntax/module-reader
  adjutor/racket/base/minus)

(provide
 (except-out (all-from-out racket/base)
             call-with-input-file
             call-with-output-file
             date
             date*
             date*-nanosecond
             date*-time-zone-name
             date*?
             date-day
             date-dst?
             date-hour
             date-minute
             date-month
             date-second
             date-time-zone-offset
             date-week-day
             date-year
             date-year-day
             date?
             do
             make-date
             make-date*
             struct:date
             struct:date*
             time))
