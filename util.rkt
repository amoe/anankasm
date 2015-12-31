#lang racket

(provide basename
	 shell-quote
	 make-extension-filter
         delete-directory/recursive
	 system/checked)

(define (delete-directory/recursive path)
  #t)


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

(define (make-extension-filter extension)
  (let ((extension-as-bytes (string->bytes/utf-8 extension)))
    (lambda (path)
      (bytes=? (filename-extension path) extension-as-bytes))))

(define (system/checked command)
  (let ((result (system/exit-code command)))
    (when (not (= result 0))
      (raise-user-error "command failed with code" result))))
