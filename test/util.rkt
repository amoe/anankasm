#lang racket/base

(require racket/sequence
	 racket/path
	 racket/string
	 racket/system
	 racket/port
	 racket/match
	 racket/list)


; These are second order level test utilities and do not themselves have tests

(provide get-track-count-from-cd-toc
	 count-files-in-directory
	 basename
	 valid-wav?
	 get-wav-duration
	 get-track-durations-from-cd-toc)

(define (get-track-count-from-cd-toc)
  ; Call cdparanoia and break the output into lines and grep it.
  (match (system-with-output-and-exit-code (format "cdparanoia -Q 2>&1"))
    ((list output exit-code)
     (count (lambda (line)
	      (regexp-match? #px"^\\s*\\d+\\." line))
	    (string-split output (string #\newline))))))

(define (count-files-in-directory path)
  (length (directory-list path)))

; remove extension from path and return a path
(define (basename path)
  (file-name-from-path
   (string-trim path
		(string-append "."
			       (bytes->string/locale
				(filename-extension path)))
		#:left? #f)))

(define (valid-wav? path)
  (match (system-with-output-and-exit-code (format "soxi -t ~a" path))
    ((list output exit-code)
     (and
      (string=? (string-trim output) "wav")
      (= exit-code 0)))))

(define (get-wav-duration path)
  (match (system-with-output-and-exit-code (format "soxi -D ~a" path))
    ((list output exit-code)
     (string->number (string-trim output)))))

(define (get-track-durations-from-cd-toc)
  '())

(define (system-with-output-and-exit-code str)
    (match (process str)
    ((list standard-output
	   standard-input
	   pid
	   standard-error
	   control)
     (close-output-port standard-input)
     (close-input-port standard-error)
     (control 'wait)
     (let ((output (port->string standard-output)))
       (close-input-port standard-output)
       (list output (control 'exit-code))))))

