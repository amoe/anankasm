#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 racket/file
	 racket/list
	 "../encode.rkt"
	 (prefix-in util: "util.rkt"))

(define input-directory (make-temporary-file "encoder-input-~a" 'directory))
(define output-directory (make-temporary-file "encoder-output-~a" 'directory))

(define-test-suite anankasm/encode
  (test-case
   "FLAC files appear in directory with correct names"
   (define number-tracks 2)
   
   (util:generate-tones number-tracks)
   (encode input-directory output-directory)

   ; check the ordering
   (let ((expected-list (build-list number-tracks add1)))
     (check-true (andmap (lambda (path n)
			   (= (string->number (util:basename path))
			      n))
			 (util:get-flacs-in-directory output-directory)
			 expected-list))))


  (test-case
   "Files are in a valid FLAC format"
   (util:generate-tones 1)
   (encode input-directory output-directory)
   (check-true
    (sequence-andmap util:is-valid-flac? (in-directory output-directory))))
   
  (test-case
   "Files are dithered down from 24-bit"
   (util:generate-tones 1 #:bit-rate 24)
   (encode input-directory output-directory)
   (let ((output-file (first (util:get-flacs-in-directory output-directory))))
     (check-equal? (util:find-bit-rate output-file) 16)))
  
  (test-case
   "Files are downsampled from 48KHz"
   (util:generate-tones 1 #:sample-rate 48000)
   (encode input-directory output-directory)
   (let ((output-file (first (util:get-flacs-in-directory output-directory))))
     (check-equal? (util:find-sample-rate output-file) 44100))))
   
(run-tests anankasm/encode)
