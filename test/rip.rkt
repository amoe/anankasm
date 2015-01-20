#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 "../rip.rkt"
	 (prefix-in util: "util.rkt"))

; We could use CDemu to test.

(define unique-id "FROM-TEST-SUITE")
(define expected-output-path (build-path "/home/amoe/.anankasm/rip" unique-id))

(define (set-up)
  (rip unique-id))


(define (tear-down)
  (display "Cleaning up\n"))

(define-test-suite anankasm/rip #:before set-up #:after tear-down
  (test-case
   "Output directory was created"
   (directory-exists? expected-output-path))

  (test-case
   "Track count matches TOC"
   (let ((track-count (util:get-track-count-from-cd-toc)))
     (check-equal? (length (util:get-wavs-in-directory expected-output-path))
		   track-count)))

  (test-case
   "Tracks are named sequentially"
   ; Sort directory list, generate integer sequence, iterate in parallel,
   ; terminate on every.
   (let ((expected-list (build-list (util:get-track-count-from-cd-toc)
				    add1)))
     (check-true (andmap (lambda (path n)
			   (= (string->number (util:basename path))
			      n))
			 (util:get-wavs-in-directory expected-output-path)
			 expected-list))))


  (test-case
   "Tracks are in valid WAV format"
   (sequence-andmap util:valid-wav? (in-directory expected-output-path)))

  (test-case
   "Track times match CD TOC"
   (let ((normalize-second (compose inexact->exact round util:second->sector)))
     (check-equal? 
      (map (compose normalize-second
		    util:get-wav-duration)
	   (util:get-wavs-in-directory expected-output-path))
      (map normalize-second (util:get-track-durations-from-cd-toc))))))


(run-tests anankasm/rip)
