#lang racket/base

;; Any helper functions that the report macros expand into should be
;; defined in this file.

;; Type annotations for these helpers should go in
;; typed/debug/report/helpers.rkt

(provide pass-through-values
         effect/report
         effect/report/line
         effect/report/file)

(require racket/match
         racket/struct
         pretty-format)

;; pass-through-values :
;; (∀ (X ...)
;;   (-> (-> (values X ...))
;;       (-> (Listof Any) Void)
;;       (values X ...)))
(define (pass-through-values thunk effect)
  (let ([lst (call-with-values thunk list)])
    (effect lst)
    (apply values lst)))

;; effect/report : Any -> [(Listof Any) -> Void]
(define ((effect/report name) expr-results)
  (pretty-eprintf "~a = ~v\n"
                  name (show-results expr-results)))

;; effect/report/line : Any Natural -> [(Listof Any) -> Void]
(define ((effect/report/line name line) expr-results)
  (pretty-eprintf "~a = ~v on line ~a\n"
                  name (show-results expr-results) line))

;; effect/report/file : Any Natural Any -> [(Listof Any) -> Void]
(define ((effect/report/file name line file) expr-results)
  (pretty-eprintf "~a = ~v on line ~a in ~v\n"
                  name (show-results expr-results) line file))

;; -------------------------------------------------------------------

(struct printed-values (vs)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'values)
      (λ (self) (printed-values-vs self))))])

;; show-results : (Listof Any) -> Any
(define (show-results expr-results)
  (match expr-results
    [(list expr-result)  expr-result]
    [_                   (printed-values expr-results)]))

