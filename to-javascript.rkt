#lang plait
(require "io.rkt")
(require "utilities.rkt")
(require (typed-in racket [number->string : (Number -> String)]))
(require (typed-in racket/string [string-replace : (String String String -> String)]))

(define (SubExpr) (ToExpr))
(define (Tail) (ToStmt (string-prefix "return ")))
(define (Ignored) (ToStmt identity))

(define (smol->javascript [program : Program]): String
  (let* ([def* (fst program)]
         [exp* (snd program)])
    (string-append (def*->string def*)
                   (top-exp*->string exp*))))

(define (def*->string [def* : (Listof Def)])
  (string-concat (map def->string def*)))

(define (top-exp*->string [exp* : (Listof Expr)])
  (string-concat (map (string-wrap "console.log(" ");") (map (exp->string (SubExpr)) exp*))))

(define (def->string [def : Def]) : String
  (type-case Def def
    [(d-fun fun arg* def* prelude* result)
     (string-concat
      (list
       "function "
       (id->string fun)
       "("
       (fun-head->string arg*)
       "){"
       (fun-body->string def* prelude* result)
       "}"))]
    [(d-var var val)
     (string-concat
      (list
       "let "
       (id->string var)
       " = "
       ((exp->string (SubExpr)) val)
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
  (kebab->camel (symbol->string id)))

(define (bind->def [bind : Bind]) : Def
  (d-var (fst bind) (snd bind)))

(define (return ec s)
  (type-case ExprCtx ec
    [(ToExpr) s]
    [(ToStmt f) (f s)]))

(define (make-app fun arg*)
  (case (string->symbol fun)
    [(+ - * / < <= > >=)
     ((string-wrap "(" ")") ((string-join fun) arg*))]
    [(equal?)
     ((string-wrap "(" ")") ((string-join "==") arg*))]
    [(eq?)
     ((string-wrap "(" ")") ((string-join "===") arg*))]
    [else
     (string-concat
      (list
       fun
       "("
       ((string-join ",") arg*)
       ")"))]))

(define (fun-head->string arg*)
  ((string-join ",") (map id->string arg*)))

(define (fun-body->string def* prelude* result)
  (string-concat
   (list
    (string-concat (map def->string def*))
    (string-concat (map (string-suffix ";") (map (exp->string (Ignored)) prelude*)))
    ((exp->string (Tail)) result))))

(define (exp->string [ec : ExprCtx])
  (lambda ([exp : Expr]) : String
    (type-case Expr exp
      [(e-con c)
       (return ec (constant->string c))]
      [(e-var x)
       (return ec (id->string x))]
      [(e-fun arg* def* prelude* result)
       (return ec
               (string-concat
                (list
                 "(("
                 (fun-head->string arg*)
                 ") => {"
                 (fun-body->string def* prelude* result)
                 "})")))]
      [(e-app fun arg*)
       (return ec
               (make-app ((exp->string (SubExpr)) fun) (map (exp->string (SubExpr)) arg*)))]
      [(e-let bind* def* prelude* result)
       ((exp->string ec)
        (e-app (e-fun (map var-of-bind bind*) def* prelude* result)
               (map val-of-bind bind*)))]
      [(e-set! var val)
       (type-case ExprCtx ec
         [(Ignored)
          (string-concat
           (list
            (id->string var)
            " = "
            ((exp->string (SubExpr)) val)))]
         [else
          (return ec
                  (string-concat
                   (list
                    (id->string var)
                    " = "
                    ((exp->string (SubExpr)) val))))])]
      [(e-begin prelude* result)
       (return ec
               (string-concat
                (list
                 "("
                 (string-concat (map (string-suffix ",") (map (exp->string ec) prelude*)))
                 ((exp->string ec) result)
                 ")")))]
      [(e-if cnd thn els)
       (if (Tail? ec)
           (string-concat
            (list
             "if ("
             ((exp->string (SubExpr)) cnd)
             ") {"
             ((exp->string (Tail)) thn)
             "}else{"
             ((exp->string (Tail)) els)
             "}"))
           (string-concat
            (list
             "("
             ((exp->string ec) cnd)
             "?"
             ((exp->string ec) thn)
             ":"
             ((exp->string ec) els)
             ")")))])))

