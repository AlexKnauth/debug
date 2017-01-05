#lang racket/base

(provide make-variable-like-transformer)

(define make-variable-like-transformer
  (if (module-declared? 'syntax/transformer)
      (dynamic-require 'syntax/transformer 'make-variable-like-transformer)
      (dynamic-require 'unstable/syntax 'make-variable-like-transformer)))

