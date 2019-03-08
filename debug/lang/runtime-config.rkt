#lang racket/base

(provide configure)

(require (only-in debug/reader use-debug-readtable))

(define (configure data)
  (use-debug-readtable))
