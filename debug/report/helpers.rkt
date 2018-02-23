#lang racket/base

;; Any helper functions that the report macros expand into should be
;; defined in this file.

;; Type annotations for these helpers should go in
;; typed/debug/report/helpers.rkt

(provide stringify-results)

(require racket/string)

;; stringify-results : (Listof Any) -> String
(define (stringify-results expr-results)
  (format (if (= 1 (length expr-results))
              "~a"
              "(values ~a)")
          (string-join (for/list ([r (in-list expr-results)])
                         (format "~v" r))
                       " ")))
