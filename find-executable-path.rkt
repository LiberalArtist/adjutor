#lang racket/base

(require racket/contract
         racket/match
         racket/promise
         racket/system)

(provide (contract-out
          [current-executable-search-directories
           (parameter/c (listof path?))]
          [find-executable-path*
           (->* {path-string?}
                {(or/c #f (and/c path-string? relative-path?))
                 any/c
                 #:search (listof path?)}
                (or/c path? #f))]))

;; Inspired by:
;; https://github.com/purcell/exec-path-from-shell/blob/master/exec-path-from-shell.el

;
;                                  
;       ;;;    ;                ;; 
;     ;;       ;;               ;; 
;   ;;;;;;; ;;;;;   ;; ;     ;;;;; 
;     ;;       ;;   ;;; ;   ;   ;; 
;     ;;       ;;   ;;  ;;  ;   ;; 
;     ;;       ;;   ;;  ;; ;;   ;; 
;     ;;       ;;   ;;  ;;  ;   ;; 
;     ;;       ;;   ;;  ;;  ;   ;; 
;     ;;       ;;   ;;  ;;   ;;; ; 
;                                  
;                                                                  
 
(define (find-executable-path** program [related #f] [deepest? #f]
                                #:search path-list)
  ;; Based on the implementation of find-executable-path
  ;; from racket/private/executable-path
  ;; The find-executable-path* wrapper guards against mutation.
  (define found
    (match program
      [(? relative-path? (app split-path 'relative _ _))
       (for*/first ([base (in-list path-list)]
                    [name (in-value (build-path (path->complete-path base)
                                                program))]
                    #:when (file-exists? name))
         name)]
      [(app path->complete-path p)
       (and (file-exists? p) p)]))
  (cond
    [(and related found)
     (find-related found related deepest?)]
    [else
     found]))

;; find-related: path? path-string? any/c -> (or/c path? #f)
;; Handles the case where find-executable-path** is looking
;; for a related path, not the actual executable path.
(define (find-related exec-pth related-pth deepest?)
  (cond
    [deepest?
     (for/or ([exec-pth (in-list
                         (reverse
                          (let loop ([exec-pth exec-pth])
                            (cons exec-pth
                                  (try-resolve-path exec-pth loop null)))))])
       (try-build-related exec-pth related-pth))]
    [else
     (let loop ([exec-pth exec-pth])
       (or (try-build-related exec-pth related-pth)
           (try-resolve-path exec-pth loop #f)))]))

;; try-resolve-path : path? [(-> path? α)] [β] -> (or/c α β)
;; If path is fully resolved, returns β.
;; Otherwise, tail-calls the second arg on the absolute,
;; resolved form of path.
(define (try-resolve-path orig [sucess-k values] [fail-v #f])
  (define (fixup-relative resolved)
    (cond
      [(relative-path? resolved)
       (match-define-values {base _ _}
         (split-path orig))
       (build-path base resolved)]
      [else
       resolved]))
  (define resolved
    (resolve-path orig))
  (if (equal? resolved orig)
      fail-v
      (sucess-k (fixup-relative resolved))))

;; try-build-related : path? path-string? -> (or/c #f path?)
(define (try-build-related exec-pth related-pth)
  (match-define-values {base _ _}
    (split-path exec-pth))
  (and (path? base)
       (let ([lib (build-path base related-pth)])
         (and (or (directory-exists? lib) 
                  (file-exists? lib))
              lib))))

;                                          
;                                          
;                                          
;                                          
;                                          
;                                          
;   ; ;;      ;;    ;; ;;;    ;;      ;;;  
;   ;;  ;    ;  ;   ;;;     ;;  ;   ;;   ; 
;   ;;  ;       ;;  ;;       ;      ;    ; 
;   ;;  ;;    ;;;;  ;;        ;;   ;;;;;;;;
;   ;;  ;    ;  ;;  ;;          ;;  ;      
;   ;;  ;   ;;  ;;  ;;      ;   ;   ;;   ; 
;   ;;;;     ;;; ;  ;;       ;;;      ;;;  
;   ;;                                     
;   ;;                                     
;   ;;                                     
;                                          

(define (getenv/path env)
  (environment-variables-ref env #"PATH"))

(define (current-path-var)
  (getenv/path (current-environment-variables)))

(define (get-bash-path-var [who 'get-bash-path]
                           #:search path-list)
  (define bash
    (find-executable-path** "bash" #:search path-list))
  (unless bash
    (error who
           "bash executable not found\n  search directories: ~e"
           path-list))
  (define out (open-output-bytes))
  (define err (open-output-bytes))
  (cond
    [(parameterize ([current-output-port out]
                    [current-error-port err]
                    [current-input-port (open-input-bytes #"")])
       (system* bash "--login" "-c" "printf %s \"$PATH\""))
     (bytes->immutable-bytes
      (get-output-bytes out))]
    [else
     (error who
            "bash exited abnormally\n  bash: ~e\n  message: ~e"
            bash
            (get-output-bytes err))]))


(define (parse-path-var var-bs [convention (system-path-convention-type)])
  ;; Based on path-list-string->path-list from racket/private/path-list
  (for*/list ([bs (in-list (regexp-split
                            (case convention
                              [(windows) #rx#";"]
                              [else #rx#":"])
                            var-bs))]
              ;; This is what Racket does, but see note below.
              [bs (in-value
                   (case convention
                     [(windows) (regexp-replace* #rx#"\"" bs #"")]
                     [else bs]))]
              #:unless (bytes=? bs #""))
    (bytes->path bs convention)))

#|
Parsing the PATH on Windows:

This says "s are required for paths that contain spaces:
https://docs.microsoft.com/en-us/previous-versions/tn-archive/ee692671(v=technet.10)

This says they're optional, but implies they might be
able to be used to escape semicolons:
https://blogs.msdn.microsoft.com/oldnewthing/20060929-06/?p=29533
(n.b. there is no escaping in the PATH on Unix)

What about paths that contain "?
|#

(define (environment-variables->path-list [env (current-environment-variables)]
                                          #:convention [convention (system-path-convention-type)]
                                          #:prefix-current-directory? [prefix? (eq? 'windows convention)])
  (define bs (getenv/path env))
  (define parsed
    (if bs
        (parse-path-var bs convention)
        null))
  (if prefix?
      (cons (bytes->path #"." convention) parsed)
      parsed))

;                                                                          
;                                                                          
;                                                                          
;                                                                          
;      ;             ;;                         ;;;                        
;      ;;            ;;                       ;;                           
;   ;;;;;   ;; ;   ;;;;;;;    ;;;   ;; ;;;  ;;;;;;;   ;;       ;;;    ;;;  
;      ;;   ;;; ;    ;;     ;;   ;  ;;;       ;;     ;  ;    ;;   ; ;;   ; 
;      ;;   ;;  ;;   ;;     ;    ;  ;;        ;;        ;;   ;      ;    ; 
;      ;;   ;;  ;;   ;;    ;;;;;;;; ;;        ;;      ;;;;  ;;     ;;;;;;;;
;      ;;   ;;  ;;   ;;     ;       ;;        ;;     ;  ;;   ;      ;      
;      ;;   ;;  ;;    ;     ;;   ;  ;;        ;;    ;;  ;;   ;;   ; ;;   ; 
;      ;;   ;;  ;;     ;;;    ;;;   ;;        ;;     ;;; ;     ;;;    ;;;  
;                                                                          
;                                                                          
;                                                                          
;                                                                          


(define (make-default-search-directories [who 'make-default-search-directories])
  (define from-env
    (environment-variables->path-list))
  (case  (system-type)
    [(macosx)
     (delay/sync
      (parse-path-var
       (get-bash-path-var who #:search from-env)))]
    [else
     from-env]))


(define current-executable-search-directories
  (make-derived-parameter
   (make-parameter (make-default-search-directories
                    'current-executable-search-directories))
   values
   force))


(define (find-executable-path* program [related #f] [deepest? #f]
                               #:search [path-list (current-executable-search-directories)])
  (define (immutable v)
    (if (string? v) (string->immutable-string v) v))
  (find-executable-path** (immutable program)
                          (immutable related)
                          deepest?
                          #:search path-list))

