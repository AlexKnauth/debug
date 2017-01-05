#lang racket/base

(require "../repl.rkt" rackunit)

(define a 3)
(define b 4)

(define-syntax-rule (with-other-vars body)
  (let ([x 5] [z 6])
    ;; x and z are not available outside this scope
    body))

(define (f x y)
  ;; x and y are local variables
  (with-other-vars
   (let ([y 7] [b 8] [c 9])
     ;; y, b, and c are local variables
     (debug-repl)
     x)))

(let ([i (open-input-string "x y a b c (+ x y a b c) (with-other-vars x)")]
      [o (open-output-string)])
  (check-equal? (parameterize ([current-input-port i]
                               [current-output-port o])
                  (f 1 2))
                1)
  (check-equal? (get-output-string o)
                (string-append
                 "-> " #;x "1\n"
                 "-> " #;y "7\n"
                 "-> " #;a "3\n"
                 "-> " #;b "8\n"
                 "-> " #;c "9\n"
                 "-> " #;(+ x y a b c) "28\n"
                 "-> " #;(with-other-vars x) "1\n"
                 "-> "))
  )

;; test for issue #9
(test-case "issue #9"
  (define (f)
    (when #true
      (debug-repl))
    (define a 1)
    a)

  (let ([i (open-input-string "y b c (+ y b c)")]
        [o (open-output-string)])
    (check-equal? (parameterize ([current-input-port i]
                                 [current-output-port o])
                    (f))
                  1)
    (check-equal? (get-output-string o)
                  (string-append
                   "-> " #;y "7\n"
                   "-> " #;b "8\n"
                   "-> " #;c "9\n"
                   "-> " #;(+ y b c) "24\n"
                   "-> ")))

  (let ([i (open-input-string "y b c (+ y b c) (+ y a b c)")]
        [o (open-output-string)])
    (check-exn #rx"a: undefined;\n cannot use before initialization"
               (λ ()
                 (parameterize ([current-input-port i]
                                [current-output-port o])
                   (f))))
    (check-equal? (get-output-string o)
                  (string-append
                   "-> " #;y "7\n"
                   "-> " #;b "8\n"
                   "-> " #;c "9\n"
                   "-> " #;(+ y b c) "24\n"
                   "-> " #;(+ y a b c)))))

