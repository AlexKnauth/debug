#lang racket/base

(provide debug-repl resume)

(require "private/make-variable-like-transformer.rkt"
         racket/list
         racket/splicing
         (for-syntax racket/base
                     racket/list
                     syntax/parse
                     pretty-format
                     ))

(define current-debug-repl-escape (make-parameter #f))

(begin-for-syntax
  ;; syntax-find-local-variables : Syntax -> (Listof Id)
  (define (syntax-find-local-variables stx)
    (define debug-info (syntax-debug-info stx (syntax-local-phase-level) #t))
    (unless (hash-has-key? debug-info 'bindings)
      (pretty-eprintf
       (string-append
        "warning: debug-repl cannot find the local bindings\n"
        "  debug-info: ~v\n")
       debug-info))
    (define context (hash-ref debug-info 'context))
    (define bindings (hash-ref debug-info 'bindings '()))
    (remove-duplicates
     (for/list ([binding (in-list bindings)]
                #:when (hash-has-key? binding 'local)
                #:when (context-subset? (hash-ref binding 'context) context))
       (datum->syntax stx (hash-ref binding 'name) stx))
     bound-identifier=?))

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
          (list (list (quote-syntax x) (λ () x)) ...)
          (list (list (quote-syntax m) mv) ...))])))

;; debug-repl/varref+hash :
;; Variable-Ref
;; (Listof (List Id (-> Any)))
;; (Listof (List Id Any))
;; ->
;; Any
(define (debug-repl/varref+hash varref var-list macro-list)
  (define ns (variable-reference->namespace varref))
  (define intro (make-syntax-introducer #true))
  (for ([pair (in-list var-list)])
    (namespace-define-transformer-binding!
     ns
     (intro (first pair))
     (make-variable-like-transformer #`(#,(second pair)))))
  (for ([pair (in-list macro-list)])
    (namespace-define-transformer-binding!
     ns
     (intro (first pair))
     (second pair)))
  (define old-prompt-read (current-prompt-read))
  (define old-eval (current-eval))
  (define (new-prompt-read)
    (write-char #\-)
    (old-prompt-read))
  (define (new-eval stx)
    (old-eval (intro stx)))
  (let/ec k
    (parameterize ([current-namespace ns]
                   [current-prompt-read new-prompt-read]
                   [current-eval new-eval]
                   [current-debug-repl-escape k])
      (read-eval-print-loop))))

;; namespace-define-transformer-binding! : Namespace Symbol Any -> Void
(define (namespace-define-transformer-binding! ns sym val)
  (eval #`(define-syntax #,(datum->syntax #f sym) #,val) ns))

;; resume : Any ... -> Nothing
(define (resume . vs)
  (define k (current-debug-repl-escape))
  (unless k
    (error 'resume "must be called within a debug-repl"))
  (apply k vs))
