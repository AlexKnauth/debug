#lang racket/base

(provide (rename-out [debug-read read]
                     [debug-read-syntax read-syntax]
                     [debug-get-info get-info]))

(require (only-in syntax/module-reader make-meta-reader)
         racket/syntax
         version/utils
         syntax/parse/define
         (for-syntax racket/base racket/list))

;; Defines the same reader syntax as the language defined by debug/lang/reader.rkt,
;; but doesn't acutually do anything

(define report-char #\R)

(define-simple-macro (require-a-lot require-spec)
  #:with [i ...] (range -10 11)
  (require (for-meta i require-spec) ...))

(require-a-lot racket/base)

(define (make-debug-readtable [rt (current-readtable)])
  (make-readtable rt report-char 'dispatch-macro report-proc))

(define (wrap-reader reader)
  (define (rd . args)
    (parameterize ([current-readtable (make-debug-readtable (current-readtable))])
      (apply reader args)))
  rd)


(define (report-proc c in src ln col pos)
  (define c2 (peek-char in))
  (define c3 (peek-char in 1))
  (cond [(and (char=? c3 report-char) (char=? c2 report-char))
         (read-char in)
         (read-char in)
         (read-syntax/recursive src in)]
        [(char=? c2 report-char)
         (read-char in)
         (read-syntax/recursive src in)]
        [else
         (read-syntax/recursive src in)]))


(define-values (debug-read debug-read-syntax debug-get-info)
  (make-meta-reader
   'debug/no-output
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
