#lang racket/base

(provide make-debug-readtable
         use-debug-readtable
         current-syntax-introducer
         wrap-reader)

(require (only-in syntax/module-reader make-meta-reader)
         racket/syntax
         version/utils
         syntax/parse/define
         (for-syntax racket/base racket/list))

(define-simple-macro (require-a-lot require-spec)
  #:with [i ...] (range -10 11)
  (require (for-meta i require-spec) ...))

(require-a-lot racket/base)


;; originally from mbutterick/sugar, sugar/debug.rkt, reader submodule
;; https://github.com/mbutterick/sugar/blob/0ffe3173879cef51d29b4c91a336a4de6c3f8ef8/sugar/debug.rkt

(define report-char #\R)
(define name-char #\N)


(define (make-debug-readtable [rt (current-readtable)])
  (make-readtable rt report-char 'dispatch-macro report-proc))


(define (use-debug-readtable [orig-rt (current-readtable)])
  (port-count-lines! (current-input-port))
  (current-readtable (make-debug-readtable orig-rt)))


;; current-syntax-introducer : (Parameterof [Syntax -> Syntax])
(define current-syntax-introducer
  (make-parameter (λ (x) x)))

;; current-intro-id-syntax : (Parameterof (U #false Syntax))
;; A value of #false means that it is not nested within another debug expression
;; A syntax object means that it is nested within another debug expression,
;; where the macros are already bound, and it should use `datum->syntax` with
;; the syntax object value as the lexical context context.
(define current-intro-id-syntax
  (make-parameter #false))

;; Any -> Any
(define (add-language-info-prop stx)
  (cond
    [(syntax? stx)
     (define old-prop (syntax-property stx 'module-language))
     (define new-prop `#(debug/lang/language-info get-language-info ,old-prop))
     (syntax-property stx 'module-language new-prop)]
    [else stx]))

(define (wrap-reader reader)
  (define (rd . args)
    (define intro
      (cond [(procedure-arity-includes? make-syntax-introducer 1)
             (make-syntax-introducer #t)]
            [else
             (make-syntax-introducer)]))
    (parameterize ([current-readtable (make-debug-readtable (current-readtable))]
                   [current-syntax-introducer intro])
      (define stx (add-language-info-prop (apply reader args)))
      (if (and (syntax? stx) (version<=? "6.2.900.4" (version)))
          (intro stx)
          stx)))
  rd)

;; -------------------------------------------------------------------

;; maybe-add-let
(define (maybe-add-local-require ctxt debug-expr)
  (define/with-syntax expr debug-expr)
  (cond [(current-intro-id-syntax)
         #'expr]
        [else
         (define/with-syntax debug/report (datum->syntax ctxt 'debug/report))
         #'(let ()
             (local-require debug/report)
             expr)]))


(define (report-proc c in src ln col pos)
  (define c2 (peek-char in))
  (define c3 (peek-char in 1))
  (define c4 (peek-char in 2))
  (define intro (current-syntax-introducer))
  (cond [(and (char=? c2 report-char) (char=? c3 report-char) (char=? c4 name-char))
         (read-char in)
         (read-char in)
         (read-char in)
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report/file (datum->syntax #'ctxt 'report/file))
         (define/with-syntax name (intro (read-syntax/recursive src in)))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report/file stx name)))]
        [(and (char=? c2 report-char) (char=? c3 name-char))
         (read-char in)
         (read-char in)
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report/line (datum->syntax #'ctxt 'report/line))
         (define/with-syntax name (intro (read-syntax/recursive src in)))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report/line stx name)))]
        [(char=? c2 name-char)
         (read-char in)
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report (datum->syntax #'ctxt 'report))
         (define/with-syntax name (intro (read-syntax/recursive src in)))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report stx name)))]
        [(and (char=? c3 report-char) (char=? c2 report-char))
         (read-char in)
         (read-char in)
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report/file (datum->syntax #'ctxt 'report/file))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report/file stx)))]
        [(char=? c2 report-char)
         (read-char in)
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report/line (datum->syntax #'ctxt 'report/line))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report/line stx)))]
        [else
         (define/with-syntax ctxt (or (current-intro-id-syntax) #'here))
         (define/with-syntax report (datum->syntax #'ctxt 'report))
         (define/with-syntax stx
           (parameterize ([current-intro-id-syntax #'ctxt])
             (intro (read-syntax/recursive src in))))
         (intro
          (maybe-add-local-require #'ctxt
                                   #'(report stx)))]))

