#lang racket/base

(provide make-variable-like-transformer)

(define make-variable-like-transformer
  (with-handlers
    ([exn:fail:filesystem:missing-module?
      (λ (e)
        (dynamic-require 'unstable/syntax
                         'make-variable-like-transformer))])
  (dynamic-require 'syntax/transformer
                   'make-variable-like-transformer)))

