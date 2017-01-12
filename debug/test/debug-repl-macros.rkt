#lang racket/base

(require "../repl.rkt"
         "test-util.rkt"
         rackunit
         (for-syntax racket/base syntax/parse))

(define a 3)
(define b 4)

(test-case "local macros that don't refer to other macros"
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
         (?list-helper #'[] #'[x ...])]
        [stx
         ;; TODO: figure out how to make syntax-parse's own errors
         ;;       not cause infinite loops
         (raise-syntax-error #f "bad syntax" #'stx)]))
    (debug-repl)
    tmp)

  (test-with-io
   #:i [i (open-input-string "a b tmp (?list a b tmp)")]
   #:o [o (open-output-string)]
   (check-equal? (f 1) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;a "3\n"
                  "-> " #;b "4\n"
                  "-> " #;tmp "1\n"
                  "-> " #;(?list a b tmp) "'(3 4 1)\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "(?list . bluh)")]
   #:o [o (open-output-string)]
   (check-exn #rx"\\?list: bad syntax"
              (λ () (f 1)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;(?list . bluh)))))

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
   #:i [i (open-input-string "a b tmp (?list a b tmp)")]
   #:o [o (open-output-string)]
   (check-equal? (f 1) 1)
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;a "3\n"
                  "-> " #;b "4\n"
                  "-> " #;tmp "1\n"
                  "-> " #;(?list a b tmp) "'(3 4 1)\n"
                  "-> ")))

  (test-with-io
   #:i [i (open-input-string "a b tmp (+ a b tmp)")]
   #:o [o (open-output-string)]
   (check-exn #rx"a: undefined;\n cannot use before initialization"
              (λ () (f 1)))
   (check-equal? (get-output-string o)
                 (string-append
                  "-> " #;b "4\n"
                  "-> " #;(+ b 13) "17\n"
                  "-> " #;(+ a b 13)))))
