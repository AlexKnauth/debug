#lang info

(define collection 'multi)

(define license 'MIT)

;; Require a version of racket after this commit:
;; make `variable-reference->namespace` enable top-level mode
;; d1c2daf15b8be048b5cea63d5a1d7206bfc8d43f

(define deps
  '(["base" #:version "6.6.0.3"]
    "rackunit-lib"
    "typed-racket-lib"
    "pretty-format"
    ))

(define build-deps
  '("rackunit-lib"
    "rackunit-typed"
    "scribble-lib"
    "racket-doc"
    "scribble-doc"
    ))

