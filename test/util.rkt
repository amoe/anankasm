#lang racket/base

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

(define (basename path)
  "FOO")

(define (valid-wav? path)
  #t)

(define (get-wav-duration path)
  #f)

(define (get-track-durations-from-cd-toc)
  '())
