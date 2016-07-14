#lang info

(define collection 'multi)

(define deps
  '(["base" #:version "6.3"]
    "rackunit-lib"
    ))

(define build-deps
  '("rackunit-lib"
    "scribble-lib"
    "racket-doc"
    "scribble-doc"
    ))

