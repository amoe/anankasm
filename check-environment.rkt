#! /usr/bin/racket

#lang racket

(require racket/string)

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
    (string-join (map number->string 
		      (sequence->list (in-range 1 (+ n-tracks 1))))
		 sep
		 #:after-last sep)))

(define (zero-pad n width)
  (~a n
      #:left-pad-string "0"
      #:min-width width
      #:align 'right))

(define (generate-track-statements n ts)
  (printf "TRACK ~a AUDIO\n" (zero-pad n 2))
  (printf "INDEX ~a ~a:00:00" (zero-pad n 2) (zero-pad ts 2)))

;  (with-output-to-file "mytoc.toc"
;    (thunk
;     (write (apply

(generate-cue-sheet)



