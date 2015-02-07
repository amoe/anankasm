#lang racket

(provide basename
	 shell-quote
	 is-wav?
	 is-flac?)

; remove extension from path and return a path
(define (basename path)
  (path->string
   (file-name-from-path
    (string-trim (path->string path)
		 (string-append "."
				(bytes->string/locale
				 (filename-extension path)))
		 #:left? #f))))


; Shell metacharacters, from POSIX
(define metacharacters
  "|&;<>()$`\"' \t\n*?[#~=%")

(define meta-re
  (regexp
    (string-join
     (map (compose regexp-quote string) (string->list metacharacters))
     "|")))

(define (shell-quote s)
  (regexp-replace*
    meta-re s (lambda args (string-append "\\" (first args)))))


(define (is-wav? path)
  (bytes=? (filename-extension path) #"wav"))

(define (is-flac? path)
  (bytes=? (filename-extension path) #"flac"))
