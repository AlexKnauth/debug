#lang racket/base

(require "../repl.rkt"
         "test-util.rkt"
         rackunit)

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

(test-with-io
 #:i [i (open-input-string "x y a b c (+ x y a b c) (with-other-vars x)")]
 #:o [o (open-output-string)]
 (check-equal? (f 1 2) 1)
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

  (test-with-io
   #:i [i (open-input-string "b (+ b 13)")]
   #:o [o (open-output-string)]
   (check-equal? (f) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;b "4\n"
                  "-> " #;(+ b 13) "17\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "b (+ b 13) (+ a b 13)")]
   #:o [o (open-output-string)]
   (check-exn #rx"a: undefined;\n cannot use before initialization"
              (λ () (f)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;b "4\n"
                  "-> " #;(+ b 13) "17\n"
                  "-> " #;(+ a b 13)))))

;; test for mutation
(define x-for-mutation 1)

(test-case "test for mutation"

  (define (f1 x-for-mutation)
    (debug-repl)
    x-for-mutation)

  (define (f2)
    (debug-repl)
    x-for-mutation)
  
  (test-with-io
   #:i [i (open-input-string "x-for-mutation")]
   #:o [o (open-output-string)]
   (check-equal? x-for-mutation 1)
   (check-equal? (f1 2) 2)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;x-for-mutation "2\n"
                  "-> "))
   (check-equal? x-for-mutation 1))

  (test-with-io
   #:i [i (open-input-string "x-for-mutation")]
   #:o [o (open-output-string)]
   (check-equal? x-for-mutation 1)
   (check-equal? (f2) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;x-for-mutation "1\n"
                  "-> "))
   (check-equal? x-for-mutation 1)))
  

