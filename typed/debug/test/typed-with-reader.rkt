#lang debug typed/racket
(require typed/debug/report)

(define x 2)
(define (f) #R x #R (+ x 4))
#R (f)
