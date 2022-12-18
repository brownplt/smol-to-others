#lang racket
(provide parse)
(require "io.rkt")
(require syntax/parse)
(require (only-in plait pair some none))

(define (good-identifier? x)
  (let ([cs (string->list (symbol->string x))])
    (and (andmap (or/c char-alphabetic?
                       char-numeric?
                       (one-of/c #\- #\_ #\* #\! #\?)) cs)
         (pair? cs)
         (not (char-numeric? (first cs))))))
(define keywords
  '(defvar
     deffun
     lambda
     λ
     let
     if
     and
     or
     not
     begin
     set!
     pair
     mpair
     left
     right
     set-left!
     set-right!
     mvec
     vec-len
     vec-ref
     vec-set!
     equal?
     eq?))
(define primitive-operators
  '(+
    -
    *
    /
    <
    <=
    >
    >=))
(define-syntax-class operator
  (pattern x:identifier
    #:fail-unless
    (member (syntax-e #'x) primitive-operators)
    "expecting a primitive operator"))
(define-syntax-class name
  (pattern x:identifier
    #:fail-when
    (or (member (syntax-e #'x) (append keywords primitive-operators))
        (not (good-identifier? (syntax-e #'x))))
    "expecting an identifier that is neither a keyword nor a primitive operator"))
(define-syntax-class constant
  (pattern x:string)
  (pattern x:number)
  (pattern x:boolean)
  (pattern #(c:literal ...))
  (pattern ((~datum quote) #(c:literal ...)))
  (pattern ((~datum quote) (c:literal ...))))
(define-syntax-class literal
  (pattern x:string)
  (pattern x:number)
  (pattern x:boolean)
  (pattern x:char)
  (pattern #(c:literal ...))
  (pattern #(c:literal ...))
  (pattern (c:literal ...)))
(define-syntax-class d
  (pattern ((~datum defvar) x:name v:e))
  (pattern ((~datum deffun) (x1:name x2:name ...) d1:d ... e1:e ... e2:e)))
(define-syntax-class e
  (pattern c:constant)
  (pattern x:name)
  (pattern ((~datum lambda) (x:name ...) d:d ... e1:e ... e2:e))
  (pattern ((~datum λ) (x:name ...) d:d ... e1:e ... e2:e))
  (pattern ((~datum let) ([x:name e1:e] ...) d:d ... e2:e ... e3:e))
  (pattern ((~datum if) e1:e e2:e e3:e))
  (pattern ((~datum and) e1:e ...))
  (pattern ((~datum or) e1:e ...))
  (pattern ((~datum not) e1:e))
  (pattern ((~datum begin) e1:e ... e2:e))
  (pattern ((~datum set!) x:name e))
  (pattern ((~datum pair) e1:e e2:e))
  (pattern ((~datum mpair) e1:e e2:e))
  (pattern ((~datum left) e1:e))
  (pattern ((~datum right) e1:e))
  (pattern ((~datum set-left!) e1:e e2:e))
  (pattern ((~datum set-right!) e1:e e2:e))
  (pattern ((~datum mvec) e1:e ...))
  (pattern ((~datum vec-len) e1:e))
  (pattern ((~datum vec-ref) e1:e e2:e))
  (pattern ((~datum vec-set!) e1:e e2:e e3:e))
  (pattern ((~datum equal?) e1:e e2:e))
  (pattern ((~datum eq?) e1:e e2:e))
  (pattern ((~datum +) e1:e e2:e e3:e ...))
  (pattern ((~datum -) e1:e e2:e e3:e ...))
  (pattern ((~datum *) e1:e e2:e e3:e ...))
  (pattern ((~datum /) e1:e e2:e e3:e ...))
  (pattern ((~datum <) e1:e e2:e))
  (pattern ((~datum <=) e1:e e2:e))
  (pattern ((~datum >) e1:e e2:e))
  (pattern ((~datum >=) e1:e e2:e))
  (pattern (e1:e e2:e ...)))
(define-syntax-class p
  (pattern (d1:d ... e1:e ...)))

(define (parse prog)
  (syntax-parse prog
    [(d1:d ... e1:e ...)
     (program (parse-d* #'(d1 ...))
              (parse-e* #'(e1 ...)))]))
(define (parse-x* x*) (map parse-x (syntax-e x*)))
(define (parse-x x)
  (syntax-parse x
    [x:name
     (syntax-e #'x)]))
(define (parse-e* expr*)
  (map parse-e (syntax-e expr*)))
(define (parse-x&e* x&e*)
  (map parse-x&e (syntax-e x&e*)))
(define (parse-e&e* e&e*)
  (map parse-e&e (syntax-e e&e*)))
(define (parse-x&e x&e)
  (syntax-parse x&e
    [[x:name e:e]
     (bind (parse-x #'x)
           (parse-e #'e))]))
(define (parse-e&e e&e)
  (syntax-parse e&e
    [[e1:e e2:e]
     (pair (parse-e #'e1)
           (parse-e #'e2))]))
(define (parse-d* d)
  (map parse-d (syntax-e d)))
(define (parse-d def)
  (syntax-parse def
    [((~datum defvar) x:name e:e)
     (d-var (parse-x #'x) (parse-e #'e))]
    [((~datum deffun) (x1:name x2:name ...) d1:d ... e1:e ... e2:e)
     (d-fun (parse-x #'x1)
            (parse-x* #'(x2 ...))
            (parse-d* #'(d1 ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]))
(define (parse-e expr)
  (syntax-parse expr
    [c:constant
     (e-con (parse-con #'c))]
    [x:name
     (e-var (syntax->datum #'x))]
    [((~datum lambda) (x:name ...) d:d ... e1:e ... e2:e)
     (e-fun (parse-x* #'(x ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]
    [((~datum λ) (x:name ...) d:d ... e1:e ... e2:e)
     (e-fun (parse-x* #'(x ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e1 ...))
            (parse-e #'e2))]
    [((~datum let) ([x:name e1:e] ...) d:d ... e2:e ... e3:e)
     (e-let (parse-x&e* #'([x e1] ...))
            (parse-d* #'(d ...))
            (parse-e* #'(e2 ...))
            (parse-e #'e3))]
    [((~datum if) e1:e e2:e e3:e)
     (e-if (parse-e #'e1) (parse-e #'e2) (parse-e #'e3))]
    [((~datum begin) e1:e ... e2:e)
     (e-begin (parse-e* #'(e1 ...))
              (parse-e #'e2))]
    [((~datum set!) x:name e1:e)
     (e-set! (parse-x #'x) (parse-e #'e1))]
    [((~datum mvec) e1:e ...)
     (e-app (e-var 'mvec) (parse-e* #'(e1 ...)))]
    [((~datum vec-len) e1:e)
     (e-app (e-var 'vec-len) (parse-e* #'(e1)))]
    [((~datum vec-ref) e1:e e2:e)
     (e-app (e-var 'vec-ref) (parse-e* #'(e1 e2)))]
    [((~datum vec-set!) e1:e e2:e e3:e)
     (e-app (e-var 'vec-set!) (parse-e* #'(e1 e2 e3)))]
    [((~datum pair) e1:e e2:e)
     (e-app (e-var 'pair) (parse-e* #'(e1 e2)))]
    [((~datum mpair) e1:e e2:e)
     (e-app (e-var 'mpair) (parse-e* #'(e1 e2)))]
    [((~datum left) e1:e)
     (e-app (e-var 'left) (parse-e* #'(e1)))]
    [((~datum right) e1:e)
     (e-app (e-var 'right) (parse-e* #'(e1)))]
    [((~datum set-left!) e1:e e2:e)
     (e-app (e-var 'set-left!) (parse-e* #'(e1 e2)))]
    [((~datum set-right!) e1:e e2:e)
     (e-app (e-var 'set-right!) (parse-e* #'(e1 e2)))]
    [((~datum pair?) e1:e)
     (e-app (e-var 'pair?) (parse-e* #'(e1)))]
    [((~datum equal?) e1:e e2:e)
     (e-app (e-var 'equal?) (parse-e* #'(e1 e2)))]
    [((~datum eq?) e1:e e2:e)
     (e-app (e-var 'eq?) (parse-e* #'(e1 e2)))]
    [(o:operator e1:e ...)
     (e-app (e-var (syntax-e #'o))
            (parse-e* #'(e1 ...)))]
    [(e1:e e2:e ...)
     (e-app (parse-e #'e1)
            (parse-e* #'(e2 ...)))]
    ))
(define (parse-con con)
  (syntax-parse con
    [x:number
     (c-num (syntax-e #'x))]
    [x:boolean
     (c-bool (syntax-e #'x))]
    [x:string
     (c-str (syntax-e #'x))]
    [((~datum quote) (x:literal ...))
     (c-list (map parse-literal (syntax-e #'(x ...))))]
    [((~datum quote) #(x:literal ...))
     (c-vec (map parse-literal (syntax-e #'(x ...))))]
    [#(x:literal ...)
     (c-vec (map parse-literal (syntax-e #'(x ...))))]))
(define (parse-literal con)
  (syntax-parse con
    [x:number
     (c-num (syntax-e #'x))]
    [x:boolean
     (c-bool (syntax-e #'x))]
    [x:string
     (c-str (syntax-e #'x))]
    [#(x:literal ...)
     (c-vec (map parse-literal (syntax-e #'(x ...))))]
    [(x:literal ...)
     (c-list (map parse-literal (syntax-e #'(x ...))))]))
