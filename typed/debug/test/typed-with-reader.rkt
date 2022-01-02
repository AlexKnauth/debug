#lang debug typed/racket
(require typed/debug/report)
(module+ test
  (require typed/rackunit))

(define x 2)
(define (f) #R x #R (+ x 4))

(module+ test
  (define p (open-output-string))
  (parameterize ([current-error-port p])
    #R (f))
  (check-equal? (get-output-string p) "x = 2\n(+ x 4) = 6\n(f) = 6\n"))
