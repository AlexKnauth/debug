#lang racket/base

(provide debug-repl resume)

(require "private/make-variable-like-transformer.rkt"
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     ))

(define debug-repl-prompt-tag (make-continuation-prompt-tag 'debug-repl))
(define debug-repl-abort-handler values)

;; ----------------------------------------------------------------------------

(begin-for-syntax
  ;; syntax-find-local-variables : Syntax -> (Listof Id)
  (define (syntax-find-local-variables stx)
    (define debug-info (syntax-debug-info stx (syntax-local-phase-level) #t))
    (define context (hash-ref debug-info 'context))
    (define bindings (hash-ref debug-info 'bindings))
    (for/list ([binding (in-list bindings)]
               #:when (hash-has-key? binding 'local)
               #:when (context-subset? (hash-ref binding 'context) context))
      (datum->syntax stx (hash-ref binding 'name))))

  ;; context-subset? : Context Context -> Boolean
  (define (context-subset? a b)
    ;; TODO: use an actual set-of-scopes subset function
    (list-prefix? a b))

  ;; non-macro-id? : Id -> Boolean
  (define NON-MACRO (gensym 'NON-MACRO))
  (define (non-macro-id? id)
    (eq? NON-MACRO (syntax-local-value id (λ () NON-MACRO))))
  )

(define-syntax debug-repl
  (lambda (stx)
    (syntax-parse stx
      [(debug-repl)
       #:do [(define all-vars (syntax-find-local-variables stx))
             (define-values [xs ms]
               (partition non-macro-id? all-vars))]
       #:with [x ...] xs
       #:with [m ...] ms
       #:with [mv ...] (map (λ (m)
                              (datum->syntax
                               stx
                               `(quote ,(syntax-local-value m))))
                            ms)
       #:with varref (syntax-local-introduce #'(#%variable-reference))
       #'(debug-repl/varref+hash
          varref
          (vector-immutable (cons 'x (λ () x)) ...)
          (vector-immutable (cons 'm mv) ...))])))

;; debug-repl/varref+hash :
;; Variable-Ref
;; (Vectorof (Cons Symbol (-> Any)))
;; (Vectorof (Cons Symbol Any))
;; ->
;; Any
(define (debug-repl/varref+hash varref var-vect macro-vect)
  (define ns (variable-reference->namespace varref))
  (for ([pair (in-vector var-vect)])
    (namespace-define-transformer-binding!
     ns
     (car pair)
     (make-variable-like-transformer #`(#,(cdr pair)))))
  (for ([pair (in-vector macro-vect)])
    (namespace-define-transformer-binding!
     ns
     (car pair)
     (cdr pair)))
  (define old-prompt-read (current-prompt-read))
  (define (new-prompt-read)
    (write-char #\-)
    (old-prompt-read))
  (parameterize ([current-namespace ns]
                 [current-prompt-read new-prompt-read])
    (call-with-continuation-prompt
     read-eval-print-loop
     debug-repl-prompt-tag
     debug-repl-abort-handler)))

;; namespace-define-transformer-binding! : Namespace Symbol Any -> Void
(define (namespace-define-transformer-binding! ns sym val)
  (eval #`(define-syntax #,(datum->syntax #f sym) #,val) ns))

;; resume : Any ... -> Nothing
(define (resume . vs)
  (apply abort-current-continuation debug-repl-prompt-tag vs))

