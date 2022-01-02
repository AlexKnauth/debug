#lang typed/racket

(require typed/debug/report)
(module+ test
  (require typed/rackunit))

(define x 5)

(module+ test
  (define p (open-output-string))

  (parameterize ([current-error-port p])
    (check-equal? (ann (report x) : Positive-Byte) 5))

  (check-equal? (get-output-string p) "x = 5\n"))
