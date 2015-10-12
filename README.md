debug
==
A meta-language for debugging, based on sugar/debug from [mbutterick/sugar](https://github.com/mbutterick/sugar)

documentation: http://pkg-build.racket-lang.org/doc/debug/index.html

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
