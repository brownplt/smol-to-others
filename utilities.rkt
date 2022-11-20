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