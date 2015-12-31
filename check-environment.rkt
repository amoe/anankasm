#! /usr/bin/racket

#lang racket

(require racket/string)
(require "util.rkt")

(define (is-linux?)
  (and (system-type 'os)
       (string-prefix? (system-type 'machine) "Linux")))

(define (cdemu-installed?)
  (find-executable-path "cdemu"))

(define (sox-installed?)
  (find-executable-path "sox"))


(when (not (is-linux?))
  (raise-user-error "This test suite only runs on Linux"))

(when (not (cdemu-installed?))
  (raise-user-error "Unable to find cdemu(1), please install cdemu."))

(when (not (sox-installed?))
  (raise-user-error "Unable to find sox(1), please install cdemu."))

(define (generate-cue-sheet)
  (let ((sep (string #\newline))
	(n-tracks 8))
    (string-join (map (lambda (n) (generate-track-statements n n))
		      (sequence->list (in-range 1 (+ n-tracks 1))))
		 sep
		 #:after-last sep
		 #:before-first "FILE \"blah.wav\" WAVE\n")))

(define (zero-pad n width)
  (~a n
      #:left-pad-string "0"
      #:min-width width
      #:align 'right))

(define (generate-track-statements n ts)
  (string-join 
   (list
    (format "TRACK ~a AUDIO" (zero-pad n 2))
    (format "INDEX ~a ~a:00:00" (zero-pad n 2) (zero-pad ts 2)))
   (string #\newline)))

(with-output-to-file "mytoc.toc"
  (thunk (display (generate-cue-sheet)))
  #:exists 'truncate)

(system/checked "cdemu load 0 mytoc.toc")




   




