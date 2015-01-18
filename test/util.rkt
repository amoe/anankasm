#lang racket/base

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
  14)

; remove extension from path and return a path
(define (basename path)
  (file-name-from-path
   (string-trim path
		(string-append "."
			       (bytes->string/locale
				(filename-extension path)))
		#:left? #f)))

(define (valid-wav? path)
  #t)

(define (get-wav-duration path)
  #f)

(define (get-track-durations-from-cd-toc)
  '())
