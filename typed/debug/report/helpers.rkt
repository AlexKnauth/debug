#lang s-exp typed-racket/base-env/extra-env-lang

(require debug/report/helpers)

(type-environment
 ;; type annotations for report helpers go here:
 [pass-through-values
  ;; (∀ (X ...)
  ;;   (-> (-> (values X ...))
  ;;       (-> (List X ...) Void)
  ;;       (values X ...)))

  (-polydots (x)
    (cl->*
     (->
      (-> (-values-dots (list) x 'x))
      (-> (make-ListDots x 'x) -Void)
      (-values-dots (list) x 'x))))]
 [stringify-results (-> (-lst Univ) -String)]
 )
