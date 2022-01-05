#lang scribble/manual

@(require (for-label racket/base
                     debug/repl
                     ))

@title{debug}

source code: @url{https://github.com/AlexKnauth/debug}

A racket lang-extension for debugging, based on sugar/debug.

@section{#lang debug}

@defmodule[debug #:lang]{
A lang-extension (like @racketmodname[at-exp]) that allows for quick debugging
shorthands to a program written in any racket-based language that looks at the
readtable.
}

To debug the value of an expression, simply put debug in front of the language
at the top of the file (for instance @hash-lang[] @racketmodname[debug]
@racketmodname[racket]), and put @litchar{#R}, @litchar{#RR} or @litchar{#RRR}
in front of the expression.

@itemize[
@item{@bold{@litchar{#R}} reports the value and returns it}
@item{@bold{@litchar{#RR}} reports the value with a line number and returns it}
@item{@bold{@litchar{#RRR}} reports the value with the file and line number, and returns it}
]

Examples:
@codeblock{
#lang debug racket
#R(+ 1 2)
;(+ 1 2) = 3
;3
}

@codeblock{
#lang debug racket
(+ 1 2 #R(* 3 4))
;(* 3 4) = 12
;15
}

@section{debug-repl}

@defmodule[debug/repl]

@defform[(debug-repl)]{
Creates a repl for debugging, which can access local variables in the context
where it is used.

For example a @racket[(debug-repl)] in a @racket[let] form
@codeblock[#:keep-lang-line? #f]{
#lang racket
(let ([x 1] [y 2])
  (debug-repl))
}
Will be able to access the @racket[x] and @racket[y] local variables (if
debugging information is enabled in DrRacket's
@seclink["Language" #:doc '(lib "scribblings/drracket/drracket.scrbl")
         #:indirect? #true]{
  @onscreen{Choose Language}} window, or if the program was executed using
 @exec{racket -l errortrace -t myprogram.rkt}).

It becomes much more useful in a function definition:
@codeblock[#:keep-lang-line? #f]{
#lang racket
(define (f x y)
  (debug-repl))
}
Then if you call @racket[(f 1 2)], it will create a repl where @racket[x] is
@racket[1] and @racket[y] is @racket[2].

In one of these repls, you can try evaluating different expressions. If you're
debugging a higher-order function for example, you can try out the functions
it accepts or creates with multiple sets of arguments to see how they react.
}

