#lang s-exp typed-racket/base-env/extra-env-lang

(require debug/report/helpers)

(type-environment
 ;; type annotations for report helpers go here:
 [stringify-results (-> (-lst Univ) -String)]
 )
