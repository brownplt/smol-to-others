#lang plait
(require (typed-in racket [string : (Char -> String)]))

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
  (foldl string-append "" s*))

(define (string-surfix [surfix : String])
  (lambda ([s : String]) : String
    (string-append s surfix)))

(define (string-prefix [prefix : String])
  (lambda ([s : String]) : String
    (string-append prefix s)))