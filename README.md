debug [![Build Status](https://travis-ci.org/AlexKnauth/debug.png?branch=master)](https://travis-ci.org/AlexKnauth/debug)
==
A lang-extension for debugging, based on sugar/debug from [mbutterick/sugar](https://github.com/mbutterick/sugar)

documentation: http://pkg-build.racket-lang.org/doc/debug/index.html

### `#lang debug`

To debug the value of an expression, simply put `debug` in front of the language at the top of
the file (for instance `#lang debug racket`), and put `#R`, `#RR` or `#RRR` in front of the
expression.

- `#R` reports the value and returns it
- `#RR` reports the value with a line number and returns it
- `#RRR` reports the value with the file and line number, and returns it

```racket
#lang debug racket
#R(+ 1 2)
```
Shows the output:
```
(+ 1 2) = 3
3
```

```racket
#lang debug racket
(+ 1 2 #R(* 3 4))
```
Shows the output:
```
(* 3 4) = 12
15
```

### `debug-repl`

```racket
> (require debug/repl)
> (define (f x y)
    (debug-repl))
> (f 1 2)
> ; in the debug-repl now
  x
1
> y
2
> (+ x y)
3
> ; exit the debug-repl by pressing ctrl-D
> ; back in the normal repl
  (f (λ (g a) (g a)) (list add1 4))
> ; and back in a new debug-repl
  x
#<procedure>
> y
(list #<procedure:add1> 4)
> (x string->number "3")
3
> (x (first y) (second y))
5
> (x (first y) (x (first y) (second y)))
6
> ; exit this debug-repl by pressing ctrl-D
```

