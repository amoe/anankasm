#lang racket

(provide basename)

; remove extension from path and return a path
(define (basename path)
  (path->string
   (file-name-from-path
    (string-trim (path->string path)
		 (string-append "."
				(bytes->string/locale
				 (filename-extension path)))
		 #:left? #f))))

