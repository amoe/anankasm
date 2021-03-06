#lang racket/base

(require rackunit
	 rackunit/text-ui
	 racket/sequence
	 racket/file
	 racket/list
	 "../encode.rkt"
	 "../util.rkt"
	 (prefix-in test-util: "../test-utility.rkt"))

(define (get-encoded path)
  (let ((extension (encode-format-extension (default-encode-format))))
    (test-util:get-file-type-in-directory path
				     (make-extension-filter extension))))


(define-test-suite anankasm/encode
  (test-case
   "FLAC files appear in directory with correct names"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))

   (define number-tracks 2)
   
   (test-util:generate-tones number-tracks input-directory)
   (encode input-directory output-directory)

   ; check the ordering
   (let ((expected-list (build-list number-tracks add1))
	 (flacs (get-encoded output-directory)))
     (check-equal? (length flacs) number-tracks)
     (check-true (andmap (lambda (path n)
			   (= (string->number (test-util:basename path))
			      n))
			 flacs
			 expected-list))))


  (test-case
   "Files are in a valid FLAC format"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))
   (test-util:generate-tones 1 input-directory)
   (encode input-directory output-directory)
   (let ((flacs (get-encoded output-directory)))
     (check-equal? (length flacs) 1)
     (check-true
      (andmap (lambda (path) (test-util:is-valid-encode? path
						    (encode-format-name 
						     (default-encode-format))))
	      flacs))))
   
  (test-case
   "Files are dithered down from 24-bit"
   ; Parameterize this test to FLAC, as lossy formats don't know about
   ; bit depth.
   (parameterize ((default-encode-format (find-encoder 'flac)))
     (define input-directory
       (make-temporary-file "encoder-input-~a" 'directory))
     (define output-directory
       (make-temporary-file "encoder-output-~a" 'directory))
     
     (test-util:generate-tones 1 input-directory #:bit-rate 24)
     (encode input-directory output-directory)

     (let ((flacs (get-encoded output-directory)))
       (check-equal? (length flacs) 1)
       (printf "~s\n" (first flacs))
       (check-equal? (test-util:find-bit-rate (first flacs)) 16))))

  
  (test-case
   "Files are downsampled from 48KHz"
   (define input-directory (make-temporary-file "encoder-input-~a" 'directory))
   (define output-directory
     (make-temporary-file "encoder-output-~a" 'directory))
   
   (test-util:generate-tones 1 input-directory #:sample-rate 48000)
   (encode input-directory output-directory)
   (let ((flacs (get-encoded output-directory)))
     (check-equal? (length flacs) 1)
     (check-equal? (test-util:find-sample-rate (first flacs)) 44100))))
   
(run-tests anankasm/encode)
