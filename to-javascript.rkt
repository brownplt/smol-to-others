#lang plait
(require "io.rkt")
(require "utilities.rkt")
(require (typed-in racket [number->string : (Number -> String)]))

(define (smol->javascript [program : Program]): String
  (let* ([def* (fst program)]
         [exp* (snd program)])
    (string-append (def*->string def*)
                   (exp*->string exp*))))

(define (def*->string [def* : (Listof Def)])
  (string-concat (map def->string def*)))

(define (exp*->string [exp* : (Listof Expr)])
  (string-concat (map exp->string exp*)))

(define (def->string [def : Def]) : String
  (type-case Def def
    [(d-fun fun arg* def* prelude* result)
     (def->string (d-var fun (e-fun arg* def* prelude* result)))]
    [(d-var var val)
     (string-concat
      (list
       "let "
       (symbol->string var)
       " = "
       (exp->string val)
       ";"))]))

(define (constant->string [c : Constant])
  (type-case Constant c
    [(c-void) "null"]
    [(c-str s)
     (string-concat
      (list
       "\""
       s
       "\""))]
    [(c-num n) (number->string n)]
    [(c-bool b) (if b "true" "false")]
    [(c-char c) (char->string c)]
    [(c-vec c*)
     (string-concat
      (list
       "["
       ((string-join ",") (map constant->string c*))
       "]"))]
    [(c-list c*)
     (string-concat
      (list
       "["
       ((string-join ",") (map constant->string c*))
       "]"))]
    ))

(define (id->string id)
  (symbol->string id))

(define (bind->def [bind : Bind]) : Def
  (d-var (fst bind) (snd bind)))

(define (optionof-exp->string optionof-exp)
  (type-case (Optionof '_) optionof-exp
    [(none) "null"]
    [(some e) (exp->string e)]))

(define (exp->string [exp : Expr]) : String
  (type-case Expr exp
    [(e-con c)
     (constant->string c)]
    [(e-var x)
     (id->string x)]
    [(e-fun arg* def* prelude* result)
     (string-concat
      (list
       "("
       ((string-join ",") (map id->string arg*))
       ") => {"
       (string-concat (map def->string def*))
       (string-concat (map (string-surfix ";") (map exp->string prelude*)))
       "return"
       (exp->string result)
       ";"
       "}"))]
    [(e-app fun arg*)
     (string-concat
      (list
       (exp->string fun)
       "("
       ((string-join ",") (map exp->string arg*))
       ")"))]
    [(e-let bind* def* prelude* result)
     (exp->string
      (e-app (e-fun (map var-of-bind bind*) def* prelude* result)
             (map val-of-bind bind*)))]
    [(e-let* bind* def* prelude* result)
     (if (empty? bind*)
         (exp->string (e-let (list) def* prelude* result))
         (exp->string (e-let (list (first bind*))
                             (list)
                             (list)
                             (e-let* (rest bind*) def* prelude* result))))]
    [(e-letrec bind* def* prelude* result)
     (exp->string
      (e-let (list)
             (append (map bind->def bind*) def*)
             prelude* result))]
    [(e-set! var val)
     (string-concat
      (list
       "("
       (id->string var)
       " = "
       (exp->string val)
       ", null)"))]
    [(e-begin prelude* result)
     (string-concat
      (list
       "("
       (string-concat (map (string-surfix ",") (map exp->string prelude*)))
       (exp->string result)
       ")"))]
    [(e-if cnd thn els)
     (string-concat
      (list
       "("
       (exp->string cnd)
       "?"
       (exp->string thn)
       ":"
       (exp->string els)
       ")"))]
    [(e-cond cnd-thn* els)
     (if (empty? cnd-thn*)
         (optionof-exp->string els)
         (let* ([cnd-thn (first cnd-thn*)]
                [cnd-thn* (rest cnd-thn*)]
                [cnd (fst cnd-thn)]
                [thn (snd cnd-thn)])
           (exp->string (e-if cnd thn (e-cond cnd-thn* els)))))]))

