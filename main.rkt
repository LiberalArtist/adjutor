#lang racket/base

(require "stable/require-provide.rkt")

(require-provide (provide-only "stable/require-provide.rkt")
                 "kernel.rkt"
                 "stable/define-star.rkt"
                 "stable/serialize.rkt"
                 "stable/in-value-star.rkt"
                 "stable/rx.rkt"
                 "stable/environment-variables.rkt")
