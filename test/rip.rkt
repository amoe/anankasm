#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 "../rip.rkt"
         "../util.rkt"
	 (prefix-in test-util: "../test-utility.rkt"))

(define unique-id "FROM-TEST-SUITE")
(define expected-output-path (build-path "/home/amoe/.anankasm/rip" unique-id))

; CLEAN OUT DIRECTORY!
(define (set-up)
  (display "Setting up test environment... ")
  (clean-previous-rip unique-id)  
  (test-util:make-dummy-wave)
  (test-util:make-cue-sheet)
  (test-util:load-dummy-cue-sheet)
  (display "done.\n"))


(define (get-wavs-in-directory directory)
  (test-util:get-file-type-in-directory directory (make-extension-filter "wav")))

(define (tear-down)
  (display "Cleaning up... ")
  (test-util:unload-dummy-cue-sheet)
  (test-util:clean-dummy-wave)
  (test-util:clean-cue-sheet)
  (display "done.\n"))


;; Define a separate test suite for the rip cleaner.
(define-test-suite anankasm/clean-previous-rip
  (test-case
   "Previous rip was cleaned"
   (make-directory expected-output-path)
   (close-output-port (open-output-file (build-path expected-output-path "somefile.wav")))
   (clean-previous-rip unique-id)
   (check-false (directory-exists? expected-output-path))))

;; The main test suite for the ripper.
; We always need to save away all data from the CD toc before calling 'rip',
; as morituri unconditionally ejects the CD when it finishes, effectively
; unloading the device from cdemu.
(define-test-suite anankasm/rip 
  (test-case
    "Output directory was created"
    (around (set-up)
      (rip unique-id)
      (check-pred directory-exists? expected-output-path)
      (tear-down)))

  (test-case
   "Track count matches TOC"
   (around (set-up)
   (begin
     (define track-count (test-util:get-track-count-from-cd-toc))
     (rip unique-id)
     (let ((track-count track-count))
       (check-equal? (length (get-wavs-in-directory expected-output-path))
		     track-count)))
   (tear-down)))

  (test-case
   "Tracks are named sequentially"
   (around (set-up)
   (begin
   ; Sort directory list, generate integer sequence, iterate in parallel,
   ; terminate on every.
   (define track-count (test-util:get-track-count-from-cd-toc))
   (rip unique-id)
   (let ((expected-list (build-list track-count add1)))
     (check-true (andmap (lambda (path n)
			   (= (string->number (test-util:basename path))
			      n))
			 (get-wavs-in-directory expected-output-path)
			 expected-list))))
   (tear-down)))

  (test-case
   "Tracks are in valid WAV format"
   (around (set-up)
   (begin
     (rip unique-id)
     (sequence-andmap test-util:valid-wav? (in-directory expected-output-path)))
   (tear-down)))

  (test-case
   "Track times match CD TOC"
   (around (set-up)
   (begin
   (define track-durations (test-util:get-track-durations-from-cd-toc))
   (rip unique-id)

   (let ((normalize-second (compose inexact->exact round test-util:second->sector)))
     (check-equal? 
      (map (compose normalize-second
		    test-util:get-wav-duration)
	   (get-wavs-in-directory expected-output-path))
      (map normalize-second track-durations))))   
   (tear-down))))


(run-tests anankasm/rip)
