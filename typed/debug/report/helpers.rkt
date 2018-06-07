#lang s-exp typed-racket/base-env/extra-env-lang

(require debug/report/helpers)

(type-environment
 ;; type annotations for report helpers go here:

 [pass-through-values
  ;; (∀ (X ...)
  ;;   (-> (-> (values X ...))
  ;;       (-> (Listof Any) Void)
  ;;       (values X ...)))
  (-polydots (x)
    (cl->*
     (->
      (-> (-values-dots (list) x 'x))
      (-> (-lst Univ) -Void)
      (-values-dots (list) x 'x))
     (->
      (-> ManyUniv)
      (-> (-lst Univ) -Void)
      ManyUniv)))]

 [effect/report       (-> Univ           (-> (-lst Univ) -Void))]
 [effect/report/line  (-> Univ -Nat      (-> (-lst Univ) -Void))]
 [effect/report/file  (-> Univ -Nat Univ (-> (-lst Univ) -Void))]
 )
