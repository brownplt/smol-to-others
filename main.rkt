#lang racket
(require racket/cmdline)
(require racket/system)
(require racket/string)
(require "./parse.rkt")
(require "./to-javascript.rkt")

(define (read-all-syntax)
  (let ([x (read-syntax)])
    (if (eof-object? x)
        (list)
        (cons x (read-all-syntax)))))

;;; (define (main)
;;;   (define smol-program (parse #`#,(read-all-syntax)))
;;;   (define javascript-program (smol->javascript smol-program))
;;;   (display javascript-program))

;;; (main)

(define file-to-compile
  (command-line
   #:program "compiler"
   #:args (filename)
   filename))

(unless (string-suffix? file-to-compile ".smol")
  (displayln "WARNING: the file name is not end with `.smol`."))

(define file-to-print
  (string-append file-to-compile ".js"))

(displayln (string-append "Printing to `" file-to-print "`"))

(with-input-from-file file-to-compile
  (lambda ()
    (with-output-to-file file-to-print
      (lambda ()
        (define smol-program (parse #`#,(read-all-syntax)))
        (define javascript-program (smol->javascript smol-program))
        (display javascript-program))
      #:exists 'replace)))

(displayln (string-append "Prettifying `" file-to-print "`"))
(void (system (string-append "js-beautify -r -f \"" file-to-print "\"")))