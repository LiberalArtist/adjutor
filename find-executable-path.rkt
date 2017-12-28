#lang racket/base

(require racket/runtime-path
         racket/generic
         racket/contract
         racket/bytes
         racket/system
         racket/match
         racket/port
         )

(provide preferred-PATH-spec/c
         (contract-out
          [find-executable-path*
           (->* {path-string?}
                {(or/c path-string? #f)
                 any/c}
                (or/c path? #f))]
          [current-preferred-PATH-spec
           (parameter/c preferred-PATH-spec/c)]
          [current-preferred-PATH
           (parameter/c preferred-PATH-spec/c
                        (or/c #f (and/c bytes-no-nuls?
                                        immutable?)))]
          ))

;;;;;; Even better:
;;;;;; see https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el


(define (find-executable-path* program [related #f] [deepest? #f])
  (parameterize ([current-environment-variables (current-envvars-to-use)])
    (find-executable-path program related deepest?)))

(define preferred-PATH-spec/c
  (or/c 'inherit
        #f ; means remove mapping
        bytes-no-nuls? ;should be made immutable
        string-no-nuls? ;should be made immutable
        ;(listof path?) ;still exeperimental
        ; promise?
        #;(cons/c 'append ; 'append/unique 'prefix 'prefix/unique ;; shadowing ?
                  (or/c bytes-no-nuls?
                        string-no-nuls?
                        (listof path?)))))

(define empty-envvars
  (make-environment-variables))

(define-generics preferred-PATH-value
  (ppv-get-spec preferred-PATH-value)
  (ppv-get-PATH preferred-PATH-value)
  (ppv-get-environment-variables preferred-PATH-value)
  #:fast-defaults
  ([(λ (x) (eq? 'inherit x))
    (define (ppv-get-spec preferred-PATH-value)
      'inherit)
    (define (ppv-get-PATH preferred-PATH-value)
      (envvars->PATH))
    (define (ppv-get-environment-variables preferred-PATH-value)
      (current-environment-variables))]
   [not
    (define (ppv-get-spec preferred-PATH-value)
      #f)
    (define (ppv-get-PATH preferred-PATH-value)
      #f)
    (define (ppv-get-environment-variables preferred-PATH-value)
      empty-envvars)]))

(struct overwriting-ppv (spec bs vars)
  #:transparent
  #:methods gen:preferred-PATH-value
  [(define (ppv-get-spec this)
     (overwriting-ppv-spec this))
   (define (ppv-get-PATH this)
     (overwriting-ppv-bs this))
   (define (ppv-get-environment-variables this)
     (overwriting-ppv-vars this))])

(define (string->immutable-bytes/locale str)
  (bytes->immutable-bytes
   (string->bytes/locale str)))

(define (spec->ppv spec)
  (match spec
    [(or #f 'inherit)
     spec]
    [(or (? bytes? (app bytes->immutable-bytes
                        (and spec bs)))
         (? string? (and (app string->immutable-string spec)
                         (app string->immutable-bytes/locale bs)))
         ;(listof path?)
         (and spec (app build-PATH-bytes bs)))
     (overwriting-ppv spec
                      bs
                      (make-environment-variables #"PATH" bs))]))          

(define-runtime-path printf-PATH.sh
  "printf-PATH.sh")

(define current-preferred-PATH/raw
  (make-parameter
   (spec->ppv
    (if (eq? (system-type) 'macosx)
        (parameterize ([current-error-port (open-output-nowhere)]
                       [current-input-port (open-input-string "")])
          (with-output-to-bytes
           (λ () (system* (find-executable-path "bash") "--login" printf-PATH.sh))))
        'inherit))))

(define (current-envvars-to-use)
  (ppv-get-environment-variables (current-preferred-PATH/raw)))

(define current-preferred-PATH-spec
  (make-derived-parameter
   current-preferred-PATH/raw
   spec->ppv
   ppv-get-spec))

(define current-preferred-PATH
  (make-derived-parameter
   current-preferred-PATH/raw
   spec->ppv
   ppv-get-PATH))
 
;not useful
;(system* bash "--rcfile" "/Users/philip/.bash-profile" "-i" echo-PATH.sh)

(define (envvars->PATH [vars (current-environment-variables)])
  (environment-variables-ref vars #"PATH"))

(define (parse-PATH-bytes/string str-or-bytes)
  (path-list-string->path-list str-or-bytes '()))

(define PATH-path-sep
  (if (eq? 'windows (system-path-convention-type))
      #";"
      #":"))

(define (build-PATH-bytes l-paths)
  (bytes->immutable-bytes
  (bytes-join (map path->bytes l-paths)
              PATH-path-sep)))

#;
(parameterize ([current-input-port
                (open-input-string "printf \"$PATH\"")])
  (with-output-to-bytes
   (λ ()
     (system* (find-executable-path "bash")
              "--login"))))




