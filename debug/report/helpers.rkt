#lang racket/base

;; Any helper functions that the report macros expand into should be
;; defined in this file.

;; Type annotations for these helpers should go in
;; typed/debug/report/helpers.rkt

(provide pass-through-values
         stringify-results)

(require racket/string)

;; pass-through-values :
;; (∀ (X ...)
;;   (-> (-> (values X ...))
;;       (-> (List X ...) Void)
;;       (values X ...)))
(define (pass-through-values thunk effect)
  (let ([lst (call-with-values thunk list)])
    (effect lst)
    (apply values lst)))

;; stringify-results : (Listof Any) -> String
(define (stringify-results expr-results)
  (format (if (= 1 (length expr-results))
              "~a"
              "(values ~a)")
          (string-join (for/list ([r (in-list expr-results)])
                         (format "~v" r))
                       " ")))
