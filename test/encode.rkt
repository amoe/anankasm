#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 racket/file
	 racket/list
	 "../encode.rkt"
	 (prefix-in util: "util.rkt"))


(define-test-suite anankasm/encode
  (test-case
   "FLAC files appear in directory with correct names"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))

   (define number-tracks 2)
   
   (util:generate-tones number-tracks input-directory)
   (encode input-directory output-directory)

   ; check the ordering
   (let ((expected-list (build-list number-tracks add1))
	 (flacs (util:get-flacs-in-directory output-directory)))
     (check-equal? (length flacs) number-tracks)
     (check-true (andmap (lambda (path n)
			   (= (string->number (util:basename path))
			      n))
			 flacs
			 expected-list))))


  (test-case
   "Files are in a valid FLAC format"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))
   (util:generate-tones 1 input-directory)
   (encode input-directory output-directory)
   (let ((flacs (util:get-flacs-in-directory output-directory)))
     (check-equal? (length flacs) 1)
     (check-true
      (andmap util:is-valid-flac? flacs))))
   
  (test-case
   "Files are dithered down from 24-bit"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))
   
   (util:generate-tones 1 input-directory #:bit-rate 24)
   (encode input-directory output-directory)

   (let ((flacs (util:get-flacs-in-directory output-directory)))
     (check-equal? (length flacs) 1)
     (check-equal? (util:find-bit-rate (first flacs)) 16)))

  
  (test-case
   "Files are downsampled from 48KHz"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))
   
   (util:generate-tones 1 input-directory #:sample-rate 48000)
   (encode input-directory output-directory)
   (let ((flacs (util:get-flacs-in-directory output-directory)))
     (check-equal? (length flacs) 1)
     (check-equal? (util:find-sample-rate (first flacs)) 44100))))
   
(run-tests anankasm/encode)
