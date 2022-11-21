#lang racket
(require racket/cmdline)
(require racket/system)
(require racket/string)
(require "./parse.rkt")
;;; (require (only-in "./to-javascript.rkt" smol->javascript))
(require (only-in "./to-python.rkt" smol->python))

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

(define files-to-compile
  (command-line
   #:program "compiler"
   #:args filenames
   filenames))

;;; (define (compile-to-javascript)
;;;   (for ([file-to-compile files-to-compile])

;;;     (unless (string-suffix? file-to-compile ".smol")
;;;       (displayln "WARNING: the file name is not end with `.smol`."))

;;;     (define file-to-print
;;;       (string-append file-to-compile ".js"))

;;;     (displayln (string-append "Printing to `" file-to-print "`"))

;;;     (with-input-from-file file-to-compile
;;;       (lambda ()
;;;         (with-output-to-file file-to-print
;;;           (lambda ()
;;;             (define smol-program (parse #`#,(read-all-syntax)))
;;;             (define javascript-program (smol->javascript smol-program))
;;;             (display javascript-program))
;;;           #:exists 'replace)))

;;;     (displayln (string-append "Prettifying `" file-to-print "`"))
;;;     (void (system (string-append "js-beautify -r -f \"" file-to-print "\"")))
;;;     ))

(define (compile-to-python)
  (for ([file-to-compile files-to-compile])

    (unless (string-suffix? file-to-compile ".smol")
      (displayln "WARNING: the file name is not end with `.smol`."))

    (define file-to-print
      (string-append file-to-compile ".py"))

    (displayln (string-append "Printing to `" file-to-print "`"))

    (with-input-from-file file-to-compile
      (lambda ()
        (with-output-to-file file-to-print
          (lambda ()
            (define smol-program (parse #`#,(read-all-syntax)))
            (define python-program (smol->python smol-program))
            (display python-program))
          #:exists 'replace)))

    (displayln (string-append "Prettifying `" file-to-print "`"))
    (void (system (string-append "autopep8 --in-place --aggressive --aggressive \"" file-to-print "\"")))
    ))

;;; (compile-to-javascript)
(compile-to-python)
