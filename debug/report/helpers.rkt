#lang racket/base

;; Any helper functions that the report macros expand into should be
;; defined in this file.

;; Type annotations for these helpers should go in
;; typed/debug/report/helpers.rkt

(provide pass-through-values
         effect/report
         effect/report/line
         effect/report/file)

(require racket/string)

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
  (eprintf "~a = ~a\n"
           name (stringify-results expr-results)))

;; effect/report/line : Any Natural -> [(Listof Any) -> Void]
(define ((effect/report/line name line) expr-results)
  (eprintf "~a = ~a on line ~a\n"
           name (stringify-results expr-results) line))

;; effect/report/file : Any Natural Any -> [(Listof Any) -> Void]
(define ((effect/report/file name line file) expr-results)
  (eprintf "~a = ~a on line ~a in ~v\n"
           name (stringify-results expr-results) line file))

;; -------------------------------------------------------------------

;; stringify-results : (Listof Any) -> String
(define (stringify-results expr-results)
  (format (if (= 1 (length expr-results))
              "~a"
              "(values ~a)")
          (string-join (for/list ([r (in-list expr-results)])
                         (format "~v" r))
                       " ")))
