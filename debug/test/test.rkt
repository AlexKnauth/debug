#lang debug racket/base

;; originally from mbutterick/sugar, sugar/test/debug-meta-lang.rkt
;; https://github.com/mbutterick/sugar/blob/0ffe3173879cef51d29b4c91a336a4de6c3f8ef8/sugar/test/debug-meta-lang.rkt

(require rackunit
         (for-meta 1 (only-in racket/base begin-for-syntax))
         (for-meta 2 (only-in racket/base begin-for-syntax))
         (for-meta 3 (only-in racket/base let #%app open-output-string get-output-string parameterize
                              current-error-port #%datum)
                   rackunit))

(let ([out (open-output-string)]
      [let "something else"]
      [local-require "something else entirely"]
      [only-in "completely unexpected!"]
      [report "well, not really"])
  (parameterize ([current-error-port out])
    #R5
    #RN x 5)
  (check-equal? (get-output-string out) "5 = 5\nx = 5\n"))

(let ([out (open-output-string)]
      [report/line "outta the blue!"])
  (parameterize ([current-error-port out])
    #RR5
    #RRN x 5)
  (check-equal? (get-output-string out) "5 = 5 on line 26\nx = 5 on line 27\n"))

(let ([out (open-output-string)])
  (parameterize ([current-error-port out])
    #RR (+ 1 2 #R 5
           #RN x (* 1 2 3)))
  (check-equal? (get-output-string out)
                "5 = 5\nx = 6\n(+ 1 2 (report 5) (report (* 1 2 3) x)) = 14 on line 32\n"))

(begin-for-syntax
  (begin-for-syntax
    (begin-for-syntax
      (let ([out (open-output-string)])
        (parameterize ([current-error-port out])
          #RR5)
        (check-equal? (get-output-string out) "5 = 5 on line 42\n")))))
