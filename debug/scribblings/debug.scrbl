#lang scribble/manual

@title{debug}

source code: @url{https://github.com/AlexKnauth/debug}

A racket meta-language for debugging, based on sugar/debug.

@section{#lang debug}

@defmodule[debug #:lang]{
A meta-language (like @racketmodname[at-exp]) that allows for quick debugging
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
