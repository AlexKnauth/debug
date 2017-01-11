#lang racket/base

(require "../repl.rkt"
         "test-util.rkt"
         rackunit
         (for-syntax racket/base syntax/parse))

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

(test-case "local macros that refer to other macros"
  (define (f tmp)
    (define-syntax ?list
      (syntax-parser
        [(?list x:expr ...)
         (define (?list-helper acc xs)
           (syntax-parse (list acc xs)
             [([acc:id ...] []) #'(list acc ...)]
             [([acc:id ...] [x:expr y:expr ...])
              #:with [tmp] (generate-temporaries #'[x])
              #`(let ([tmp x])
                  (if tmp
                      #,(?list-helper #'[acc ... tmp] #'[y ...])
                      #false))]))
         (?list-helper #'[] #'[x ...])]))
    (debug-repl)
    tmp)

  (test-with-io
   #:i [i (open-input-string "y a b c tmp (?list y a b c tmp)")]
   #:o [o (open-output-string)]
   (check-equal? (f 1) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;y "7\n"
                  "-> " #;a "3\n"
                  "-> " #;b "8\n"
                  "-> " #;c "9\n"
                  "-> " #;tmp "1\n"
                  "-> " #;(?list y a b c tmp) "'(7 3 8 9 1)\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "(?list . bluh)")]
   #:o [o (open-output-string)]
   (check-exn #rx"\\?list: bad syntax"
              (λ () (f 1)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;(?list. bluh)))))

;; TODO: !!! identifier used out of context !!!
#;
(test-case "local macros that refer to other macros"
  (define (f tmp)
    (define-syntax ?list-helper
      (syntax-parser
        [(?list-helper [acc:id ...] []) #'(list acc ...)]
        [(?list-helper [acc:id ...] [x:expr y:expr ...])
         #'(let ([tmp x])
             (if tmp
                 (?list-helper [acc ... tmp] [y ...])
                 #false))]))
    (define-syntax-rule (?list x ...)
      (?list-helper [] [x ...]))
    (debug-repl)
    tmp)

  (test-with-io
   #:i [i (open-input-string "y a b c tmp (?list y a b c tmp)")]
   #:o [o (open-output-string)]
   (check-equal? (f 1) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;y "7\n"
                  "-> " #;a "3\n"
                  "-> " #;b "8\n"
                  "-> " #;c "9\n"
                  "-> " #;tmp "1\n"
                  "-> " #;(?list y a b c tmp) "'(7 3 8 9 1)\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "y a b c tmp (+ y a b c tmp)")]
   #:o [o (open-output-string)]
   (check-exn #rx"a: undefined;\n cannot use before initialization"
              (λ () (f 1)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;y "7\n"
                  "-> " #;b "8\n"
                  "-> " #;c "9\n"
                  "-> " #;(+ y b c) "24\n"
                  "-> " #;(+ y a b c)))))

;; test for issue #9
(test-case "issue #9"
  (define (f)
    (when #true
      (debug-repl))
    (define a 1)
    a)

  (test-with-io
   #:i [i (open-input-string "y b c (+ y b c)")]
   #:o [o (open-output-string)]
   (check-equal? (f) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;y "7\n"
                  "-> " #;b "8\n"
                  "-> " #;c "9\n"
                  "-> " #;(+ y b c) "24\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "y b c (+ y b c) (+ y a b c)")]
   #:o [o (open-output-string)]
   (check-exn #rx"a: undefined;\n cannot use before initialization"
              (λ () (f)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;y "7\n"
                  "-> " #;b "8\n"
                  "-> " #;c "9\n"
                  "-> " #;(+ y b c) "24\n"
                  "-> " #;(+ y a b c)))))

