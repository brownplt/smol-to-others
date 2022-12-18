#lang plait
(require "io.rkt")
(require "utilities.rkt")
(require (typed-in racket [number->string : (Number -> String)]))
(require (typed-in racket/string [string-replace : (String String String -> String)]))

(define (smol->javascript [program : Program]): String
  (let* ([def* (fst program)]
         [exp* (snd program)])
    (string-concat-line
     (append
      (map def->string def*)
      (map (exp->string (as-stmt (top))) exp*)))))

(define string-concat-line (string-join "\n"))

(define (def->string [def : Def]) : String
  (type-case Def def
    [(d-fun fun arg* def* prelude* result)
     (string-concat
      (list
       "function "
       (id->string fun)
       "("
       (fun-head->string arg*)
       ") {\n"
       (indent (fun-body->string def* prelude* result))
       "}"))]
    [(d-var var val)
     (string-concat
      (list
       "let "
       (id->string var)
       " = "
       ((exp->string (as-expr)) val)
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
  (kebab->jsvar (symbol->string id)))

(define (bind->def [bind : Bind]) : Def
  (d-var (fst bind) (snd bind)))

(define (make-vec-set! ec v i e)
  (return ec
          (string-concat (list v "[" i "] = " e))))

(define (make-app ec [fun : Expr] arg*)
  (type-case Expr fun
    [(e-var x)
     (case x
       [(set-left!)
        (make-vec-set! ec (first arg*) "0" (second arg*))]
       [(set-right!)
        (make-vec-set! ec (first arg*) "1" (second arg*))]
       [(vec-set!)
        (make-vec-set! ec (first arg*) (second arg*) (third arg*))]
       [else
        (return ec (case x
                     [(+ - * / < <= > >=)
                      ((string-wrap "(" ")") ((string-join (symbol->string x)) arg*))]
                     [(equal?)
                      ((string-wrap "(" ")") ((string-join "==") arg*))]
                     [(eq?)
                      ((string-wrap "(" ")") ((string-join "===") arg*))]
                     [(pair mpair)
                      (string-concat
                       (list
                        "["
                        (first arg*)
                        ","
                        (second arg*)
                        "]"))]
                     [(left)
                      (string-concat
                       (list
                        (first arg*)
                        "[0]"))]
                     [(right)
                      (string-concat
                       (list
                        (first arg*)
                        "[1]"))]
                     [(vec mvec)
                      ((string-wrap "[" "]")
                       ((string-join ", ") arg*))]
                     [(vec-len)
                      (string-concat
                       (list
                        (first arg*)
                        ".length"))]
                     [(vec-ref)
                      (string-concat
                       (list
                        (first arg*)
                        "["
                        (second arg*)
                        "]"))]
                     [else
                      (string-concat
                       (list
                        (id->string x)
                        "("
                        ((string-join ",") arg*)
                        ")"))]))])]
    [else
     (return ec (string-concat
                 (list
                  ((exp->string (as-expr)) fun)
                  "("
                  ((string-join ",") arg*)
                  ")")))]))

(define (fun-head->string arg*)
  ((string-join ",") (map id->string arg*)))

(define (fun-body->string def* prelude* result)
  (string-concat-line
   (append
    (map def->string def*)
    (append
     (map (exp->string (as-stmt (bgn))) prelude*)
     (list
      ((exp->string (as-stmt (ret))) result))))))

(define (return ec s)
  (type-case ExprCtx ec
    [(as-expr) s]
    [(as-stmt stmt)
     (type-case Stmt stmt
       [(top) ((string-wrap "console.log(" ");") s)]
       [(ret) ((string-wrap "return " ";") s)]
       [(bgn) s])]))

(define (stmt-as-stmt stmt s)
  (type-case Stmt stmt
    [(top) s]
    [(ret) (string-append s "\nreturn;")]
    [(bgn) s]))

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
                 "("
                 (fun-head->string arg*)
                 ") => {"
                 (fun-body->string def* prelude* result)
                 "}")))]
      [(e-app fun arg*)
       (make-app ec fun (map (exp->string (as-expr)) arg*))]
      [(e-let bind* def* prelude* result)
       ((exp->string ec)
        (e-app (e-fun (map var-of-bind bind*) def* prelude* result)
               (map val-of-bind bind*)))]
      [(e-set! var val)
       (return ec
               (string-concat
                (list
                 (id->string var)
                 " = "
                 ((exp->string (as-expr)) val))))]
      [(e-begin prelude* result)
       (type-case ExprCtx ec
         [(as-expr)
          (string-concat
           (list
            "("
            (string-concat (map (string-suffix ",") (map (exp->string (as-expr)) prelude*)))
            ((exp->string (as-expr)) result)
            ")"))]
         [(as-stmt stmt)
          (string-concat-line
           (append
            (map (exp->string (as-stmt (bgn))) prelude*)
            (list ((exp->string (as-stmt stmt)) result))))])]
      [(e-if cnd thn els)
       (type-case ExprCtx ec
         [(as-expr)
          (string-concat
           (list
            ((exp->string ec) cnd)
            " ? "
            ((exp->string ec) thn)
            " : "
            ((exp->string ec) els)))]
         [else
          (string-concat-line
           (list
            (string-concat (list "if (" ((exp->string (as-expr)) cnd) ") {"))
            (indent ((exp->string ec) thn))
            "} else {"
            (indent ((exp->string ec) els))
            "}"))])])))
