#lang racket/base

(provide (all-defined-out))

(require "report/helpers.rkt"
         (for-syntax racket/base))

;; from mbutterick/sugar, typed/sugar/debug.rkt
;; https://github.com/mbutterick/sugar/blob/0ffe3173879cef51d29b4c91a336a4de6c3f8ef8/typed/sugar/debug.rkt
;; using normal racket/base so that it doesn't have a dependancy on typed racket

;; Identifiers referenced in the templates of these macros must be either:
;;  - defined in racket/base, or
;;  - defined in "report/helpers.rkt" with type declarations in
;;    typed/debug/report/helpers
;;
;; Otherwise these macros will break in typed racket.

(define-syntax (report stx)
  (syntax-case stx ()
    [(_ expr) #'(report expr expr)]
    [(_ expr name)
     #'(pass-through-values
        (λ () expr)
        (effect/report 'name))]))


(define-syntax (report/line stx)
  (syntax-case stx ()
    [(_ expr) #'(report/line expr expr)]
    [(_ expr name)
     (with-syntax ([line (syntax-line #'expr)])
       #'(pass-through-values
          (λ () expr)
          (effect/report/line 'name 'line)))]))


(define-syntax (report/file stx)
  (syntax-case stx ()
    [(_ expr) #'(report/file expr expr)]
    [(_ expr name)
     (with-syntax ([file (syntax-source #'expr)]
                   [line (syntax-line #'expr)])
       #'(pass-through-values
          (λ () expr)
          (effect/report/file 'name 'line 'file)))]))


(define-syntax-rule (define-multi-version multi-name name)
  (define-syntax-rule (multi-name x (... ...))
    (begin (name x) (... ...))))

(define-multi-version report* report)
(define-multi-version report*/line report/line)
(define-multi-version report*/file report/file)

