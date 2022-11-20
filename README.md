# smol-to-others

Translate SMoL programs to other programming languages

## The grammar of SMoL

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
    | (vec e ...)
    | (mvec e ...)
    | (vec-len e)
    | (vec-ref e e)
    | (vec-set! e e e)
    | (pair e e)
    | (mpair e e)
    | (left e)
    | (right e)
    | (set-left! e e)
    | (set-right! e e)
    | (pair? e)
    | (equal? e e)
    | (eq? e e)
    | (o e ...)
    | (e e ...)
o ::= < | <= | > | >=
    | + | - | * | /

body    ::= d ... e ... e
program ::= d ... e ...
```

## Know differences from the `#lang smol`

Immutable vectors are not supported. So `ivec` is not defined. `pair` is considered an alias of `mpair`.

`and`, `or`, and `not` are not suported.

`++` is not supported.

`test/not` is not supported.

`spy` is not supported.

`let*` and `letrec` are not supported.

`zero?` are not supported.

Lists are not supported. So `cons`, `empty`, `list`, `map`, `filter`, `foldl`, and `foldr` are not provided.

## Dependencies

To JavaScript:

- [`js-beautify`](https://github.com/beautify-web/js-beautify)

