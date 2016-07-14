#lang racket/base

(require "../repl.rkt" rackunit)

(define a 4)

(define-syntax-rule (with-other-vars body)
  (let ([x 5] [z 6])
    ;; x and z are not available outside this scope
    body))

(define (f x y)
  ;; x and y are local variables
  (with-other-vars
   (let ([y 7] [b 8])
     ;; y and b are local variables
     (debug-repl)
     x)))

(let ([i (open-input-string "x y a b (+ x y a b)")]
      [o (open-output-string)])
  (check-equal? (parameterize ([current-input-port i]
                               [current-output-port o])
                  (f 1 2))
                1)
  (check-equal? (get-output-string o)
                (string-append
                 "> " #;x "1\n"
                 "> " #;y "7\n"
                 "> " #;a "4\n"
                 "> " #;b "8\n"
                 "> " #;(+ x y a b) "20\n"
                 "> "))
  )

