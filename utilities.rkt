#lang plait
(require (typed-in racket [string : (Char -> String)]))
(require (typed-in racket [number->string : (Number -> String)]))
(require (typed-in racket [hash-values : ((Hashof 'a 'b) -> (Listof 'b))]))
(require (typed-in racket [string-upcase : (String -> String)]))
(require (opaque-type-in racket [Regexp regexp?]))
(require (typed-in racket [regexp : (String -> Regexp)]))
(require (typed-in racket [regexp-replace* : (Regexp String (String String -> String) -> String)]))

(define-type Stmt
  (top)
  (bgn)
  (ret))

(define-type ExprCtx
  (as-stmt [stmt : Stmt])
  (as-expr))

(define (char->string c)
  (string c))

(define (string-join sep)
  (lambda (s*)
    (if (empty? s*)
        ""
        (string-append
         (first s*)
         (string-concat (map (string-prefix sep) (rest s*)))))))

(define (string-concat s*)
  (foldl (lambda (x so-far) (string-append so-far x)) "" s*))

(define (string-suffix [suffix : String])
  (lambda ([s : String]) : String
    (string-append s suffix)))

(define (string-prefix [prefix : String])
  (lambda ([s : String]) : String
    (string-append prefix s)))

(define (string-wrap prefix suffix)
  (lambda ([s : String])
    (string-append prefix (string-append s suffix))))

(define (replace-*- s)
  (regexp-replace* (regexp "[*]") s (lambda (_s c) "s")))

(define (replace-!- s)
  (regexp-replace* (regexp "[!]") s (lambda (_s c) "-bang")))

(define (replace-?- s)
  (regexp-replace* (regexp "[?]") s (lambda (_s c) "-huh")))

(define (replace-special s)
  (replace-*- (replace-!- (replace-?- s))))

(define (propose-camel s)
  (let* ([s (regexp-replace* (regexp "[-_]([a-z])")
                             s
                             (lambda (_s c)
                               (string-upcase c)))]
         [s (regexp-replace* (regexp "([-_])")
                             s
                             (lambda (_s c)
                               ""))])
    s))

(define (propose-snake s)
  (let* ([s (regexp-replace* (regexp "([-_])")
                             s
                             (lambda (_s c)
                               "_"))])
    s))

(define (first-not-in v us)
  (local ((define (loop i)
            (local (
                    (define u (string-append v (number->string i)))
                    )
              (if (member u us)
                  (loop (add1 i))
                  u))))
    (if (member v us)
        (loop 1)
        v)))

(define (make-translater propose reserved-names fallback-name)
  (local ((define cache (make-hash (list))))
    (lambda (kebab)
      (type-case (Optionof '_) (hash-ref cache kebab)
        [(some v) v]
        [(none)
         (local ((define v (if (member kebab reserved-names)
                               fallback-name
                               (propose kebab))))
           (begin
             (hash-set! cache v (first-not-in v (hash-values cache)))
             v))]))))

(define kebab->camel (make-translater propose-camel (list) "x"))
(define kebab->jsvar (make-translater propose-camel (list "return" "var" "function") "x"))
(define kebab->snake (make-translater propose-snake (list) "x"))

(define (indent [s : String])
  (let ([p (regexp "(\n)")])
    (regexp-replace* p (string-append "    " s) (lambda (_s g) "\n    ")))
  #;
  (let ([pad "    "])
    (regexp-replace* (regexp "\n")
                     (string-append pad s)
                     (lambda (_s c) (string-append "\n" pad)))))
