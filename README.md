# smol-to-others

Translate SMoL programs to other programming languages

## The grammar of SMoL

```
d ::= (defvar x e)
   |  (deffun (f x ...) body)
e ::= c
   |  x
   |  (set! x e)
   |  (if e e e)
   |  (begin e ... e)
   |  (lambda (x ...) body)
   |  (let ([x e] ...) body)
   |  (letrec ([x e] ...) body)
   |  (let* ([x e] ...) body)
   |  (e e ...)

body    ::= d ... e ... e
program ::= d ... e ...
```