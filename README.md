# smol-to-others

Translate SMoL programs to other programming languages:

- JavaScript
- Python
- (WIP) Racket
- (WIP) OCaml

## Design decisions

Sometimes, the most "natural" translation does not preserve the semantics. For example, `(set! x 2)` produces the void value in smol while `x = 2` produces `2` in JavaScript. We decide to go for the natural translation because a goal of the whole smol project is to point out the consensus while *acknowledging* the differences.

TODO: we keep the spirit of programs -- set! and vec-set! etc. are not printed.

It is fine that some smol program can't find their translation. For example, `begin` has no obvious counterparts in Python.

## The supported subset of SMoL

### Know differences from the `#lang smol`

Immutable vectors are not supported. So `ivec` is not defined. `pair` is considered an alias of `mpair`.

`pair?` is not supported.

`and`, `or`, and `not` are not suported.

`++` is not supported.

`test/not` is not supported.

`spy` is not supported.

`let*` and `letrec` are not supported.

`zero?` are not supported.

Lists are not supported. So `cons`, `empty`, `list`, `map`, `filter`, `foldl`, and `foldr` are not provided.

Some identifiers are not allowed to be used as variables. Specifically,
a variable must not look like of the keywords and primitive operators.
Furthermore, only alphabetic characters, numeric characters, `-`, `_`, `*`, `!`, and `?` can appear in a variable. A variable must be non-empty and must not start with a number.

```
d ::= (defvar x e)
    | (deffun (f x ...) body)
e ::= c
    | x
    | (lambda (x ...) body)
    | (λ (x ...) body)
    | (let ([x e] ...) body)
    | (if e e e)
    | (begin e ... e)
    | (set! x e)
    | (pair e e)
    | (mpair e e)
    | (left e)
    | (right e)
    | (set-left! e e)
    | (set-right! e e)
    | (vec e ...)
    | (mvec e ...)
    | (vec-len e)
    | (vec-ref e e)
    | (vec-set! e e e)
    | (equal? e e)
    | (eq? e e)
    | (o e ...)
    | (e e ...)
o ::= < | <= | > | >=
    | + | - | * | /

body    ::= d ... e ... e
program ::= d ... e ...
```

## Dependencies

- (for javascript) `js-beautify`
- (for python) `autopep8`
