#lang racket/base

(provide test-with-io)

(define-syntax-rule (test-with-io
                     #:i [i input-port]
                     #:o [o output-port]
                     body ...)
  (let ([i input-port]
        [o output-port])
    (parameterize ([current-input-port i]
                   [current-output-port o])
      body ...)))

