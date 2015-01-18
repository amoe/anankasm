#lang racket/base

(require racket/sequence
	 racket/path
	 racket/string
	 racket/system
	 racket/port
	 racket/match)


; These are second order level test utilities and do not themselves have tests

(provide get-track-count-from-cd-toc
	 count-files-in-directory
	 basename
	 valid-wav?
	 get-wav-duration
	 get-track-durations-from-cd-toc)

(define (get-track-count-from-cd-toc)
  14)

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
  #f)

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

