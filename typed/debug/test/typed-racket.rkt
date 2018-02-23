#lang typed/racket

(require typed/debug/report
         typed/rackunit)

(define x 5)

(check-equal? (ann (report x) : Positive-Byte) 5)
