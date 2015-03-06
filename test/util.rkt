#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/file
	 "../util.rkt"
	 (prefix-in test-util: "../test-utility.rkt"))

; Test suite for utility procedures

(define-test-suite anankasm/util
  (test-case
   "basename has the correct output"
   (check-equal? (basename (string->path "foo.txt")) "foo"))

  (test-case
   "directory is correctly deleted"
   (define temporary-directory (make-temporary-file "delete-directory-recursive-~a"
						    'directory))

   (for ([path (in-list (map (lambda (n) (build-path temporary-directory n))
			     (map number->string (build-list 10 values))))])
     (close-output-port (open-output-file path #:exists 'truncate)))

   (delete-directory/recursive temporary-directory)
   (check-false (directory-exists? temporary-directory))))
     



(run-tests anankasm/util)

