#lang racket/base

(provide debug-repl)

(require unstable/syntax
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     ))

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
  )

(define-syntax debug-repl
  (lambda (stx)
    (syntax-parse stx
      [(debug-repl)
       #:with [x ...] (syntax-find-local-variables stx)
       #:with varref (syntax-local-introduce #'(#%variable-reference))
       #'(debug-repl/varref+hash
          varref
          (vector-immutable (cons 'x (λ () x)) ...))])))

;; debug-repl/varref+hash : Variable-Ref (Vectorof (Cons Symbol Any)) -> Any
(define (debug-repl/varref+hash varref vect)
  (define ns (variable-reference->namespace varref))
  (for ([pair (in-vector vect)])
    (namespace-define-transformer-binding!
     ns
     (car pair)
     (make-variable-like-transformer #`(#,(cdr pair)))))
  (define old-prompt-read (current-prompt-read))
  (define (new-prompt-read)
    (write-char #\-)
    (old-prompt-read))
  (parameterize ([current-namespace ns]
                 [current-prompt-read new-prompt-read])
    (read-eval-print-loop)))

;; namespace-define-transformer-binding! : Namespace Symbol Any -> Void
(define (namespace-define-transformer-binding! ns sym val)
  (eval #`(define-syntax #,(datum->syntax #f sym) #,val) ns))

