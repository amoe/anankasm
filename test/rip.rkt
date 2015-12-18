#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 "../rip.rkt"
         "../util.rkt"
	 (prefix-in test-util: "../test-utility.rkt"))

; We could use CDemu to test.

(define unique-id "FROM-TEST-SUITE")
(define expected-output-path (build-path "/home/amoe/.anankasm/rip" unique-id))


; CLEAN OUT DIRECTORY!
(define (set-up)
  (clean-previous-rip unique-id)
  (rip unique-id))

(define (get-wavs-in-directory directory)
  (test-util:get-file-type-in-directory directory (make-extension-filter "wav")))

(define (tear-down)
  (display "Cleaning up\n"))

(define-test-suite anankasm/clean-previous-rip
  (test-case
   "Previous rip was cleaned"
   (make-directory expected-output-path)
   (close-output-port (open-output-file (build-path expected-output-path "somefile.wav")))
   (clean-previous-rip unique-id)
   (check-false (directory-exists? expected-output-path))))

(define-test-suite anankasm/rip #:before set-up #:after tear-down
  (test-case
   "Output directory was created"
   (check-pred directory-exists? expected-output-path))

  (test-case
   "Track count matches TOC"
   (let ((track-count (test-util:get-track-count-from-cd-toc)))
     (check-equal? (length (get-wavs-in-directory expected-output-path))
		   track-count)))

  (test-case
   "Tracks are named sequentially"
   ; Sort directory list, generate integer sequence, iterate in parallel,
   ; terminate on every.
   (let ((expected-list (build-list (test-util:get-track-count-from-cd-toc)
				    add1)))
     (check-true (andmap (lambda (path n)
			   (= (string->number (test-util:basename path))
			      n))
			 (get-wavs-in-directory expected-output-path)
			 expected-list))))


  (test-case
   "Tracks are in valid WAV format"
   (sequence-andmap test-util:valid-wav? (in-directory expected-output-path)))

  (test-case
   "Track times match CD TOC"
   (let ((normalize-second (compose inexact->exact round test-util:second->sector)))
     (check-equal? 
      (map (compose normalize-second
		    test-util:get-wav-duration)
	   (get-wavs-in-directory expected-output-path))
      (map normalize-second (test-util:get-track-durations-from-cd-toc))))))


(run-tests anankasm/rip)
