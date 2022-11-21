#lang plait
(require "io.rkt")
(require "utilities.rkt")
(require (typed-in racket [number->string : (Number -> String)]))
(require (typed-in racket/string [string-replace : (String String String -> String)]))

(define (to-return s)
  (if (equal? s "None")
      "return"
      ((string-prefix "return ") s)))
(define (to-print s)
  (if (equal? s "None")
      ""
      ((string-wrap "print(" ")") s)))
(define to-ignore identity)

(define (smol->python [program : Program]): String
  (let* ([def* (fst program)]
         [exp* (snd program)])
    (string-concat-line
     (append
      (map def->string def*)
      (map (exp->string (ToStmt to-print)) exp*)))))

(define string-concat-line (string-join "\n"))

(define (def->string [def : Def]) : String
  (type-case Def def
    [(d-fun fun arg* def* prelude* result)
     (string-concat
      (list
       "def "
       (id->string fun)
       "("
       (fun-head->string arg*)
       "):\n"
       (indent (fun-body->string def* prelude* result))))]
    [(d-var var val)
     (string-concat
      (list
       ""
       (id->string var)
       " = "
       ((exp->string (ToExpr)) val)
       ""))]))

(define (constant->string [c : Constant])
  (type-case Constant c
    [(c-void) "None"]
    [(c-str s)
     (string-concat
      (list
       "\""
       s
       "\""))]
    [(c-num n) (number->string n)]
    [(c-bool b) (if b "True" "False")]
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
  (kebab->snake (symbol->string id)))

(define (bind->def [bind : Bind]) : Def
  (d-var (fst bind) (snd bind)))

(define (make-vec-set! ec v i e)
  (type-case ExprCtx ec
    [(ToExpr)
     (string-concat
      (list v ".__setitem__(" i ", " e ")"))]
    [(ToStmt f)
     (string-concat
      (list v "[" i "] = " e))]))

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
                      ((string-wrap "(" ")") ((string-join "is") arg*))]
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
                        "len("
                        (first arg*)
                        ")"))]
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
                  ((exp->string (ToExpr)) fun)
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
     (map (exp->string (ToStmt to-ignore)) prelude*)
     (list
      ((exp->string (ToStmt to-return)) result))))))

(define (return ec s)
  (type-case ExprCtx ec
    [(ToExpr) s]
    [(ToStmt f) (f s)]))

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
                 "lambda "
                 (fun-head->string arg*)
                 ": "
                 (fun-body->string def* prelude* result)
                 "")))]
      [(e-app fun arg*)
       (make-app ec fun (map (exp->string (ToExpr)) arg*))]
      [(e-let bind* def* prelude* result)
       ((exp->string ec)
        (e-app (e-fun (map var-of-bind bind*) def* prelude* result)
               (map val-of-bind bind*)))]
      [(e-set! var val)
      ;;;  ;;; ignore ec!!!
      ;;;  (string-concat
      ;;;   (list
      ;;;    (id->string var)
      ;;;    " = "
      ;;;    ((exp->string (ToExpr)) val)))
      ;;;  #;
       (type-case ExprCtx ec
         [(ToExpr)
          (string-concat
           (list
            (id->string var)
            " := "
            ((exp->string (ToExpr)) val)))]
         [(ToStmt f)
          (string-concat-line
           (list
            (string-concat
             (list
              (id->string var)
              " = "
              ((exp->string (ToExpr)) val)))
            ;;; ignore the context !!!
            #;(f "None")))])]
      [(e-begin prelude* result)
       (type-case ExprCtx ec
         [(ToExpr)
          (string-concat
           (list
            "("
            (string-concat (map (string-suffix ",") (map (exp->string (ToExpr)) prelude*)))
            ((exp->string (ToExpr)) result)
            ")[-1]"))]
         [else
          (string-concat-line
           (append
            (map (exp->string (ToStmt to-ignore)) prelude*)
            (list ((exp->string ec) result))))])]
      [(e-if cnd thn els)
       (type-case ExprCtx ec
         [(ToExpr)
          (string-concat
           (list
            ((exp->string ec) thn)
            " if "
            ((exp->string ec) cnd)
            " else "
            ((exp->string ec) els)))]
         [else
          (string-concat-line
           (list
            (string-concat (list "if " ((exp->string (ToExpr)) cnd) ":"))
            (indent ((exp->string ec) thn))
            "else:"
            (indent ((exp->string ec) els))))])])))

