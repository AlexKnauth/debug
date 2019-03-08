#lang racket/base

(provide (rename-out [debug-read read]
                     [debug-read-syntax read-syntax]
                     [debug-get-info get-info]))

(require (only-in syntax/module-reader make-meta-reader)
         racket/syntax
         version/utils
         syntax/parse/define
         "../reader.rkt"
         (for-syntax racket/base racket/list))

(define-values (debug-read debug-read-syntax debug-get-info)
  (make-meta-reader
   'debug
   "language path"
   (lambda (bstr)
     (let* ([str (bytes->string/latin-1 bstr)]
            [sym (string->symbol str)])
       (and (module-path? sym)
            (vector
             ;; try submod first:
             `(submod ,sym reader)
             ;; fall back to /lang/reader:
             (string->symbol (string-append str "/lang/reader"))))))
   wrap-reader
   wrap-reader
   (lambda (proc)
     (lambda (key defval)
       (define (fallback) (if proc (proc key defval) defval))
       (case key
         [else (fallback)])))))
