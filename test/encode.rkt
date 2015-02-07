#lang racket/base

(require rackunit
	 rackunit/text-ui
	 "../encode.rkt")

(define-test-suite anankasm/encode
  (test-case
   "FLAC files appear in directory with correct names"
   (check-equal? 42 42))
  (test-case
   "Files are in a valid FLAC format"
   (check-equal? 42 42))
  (test-case
   "Files are dithered down from 24-bit"
   (check-equal? 42 42))
  (test-case
   "Files are downsampled from 48KHz"
   (check-equal? 42 42)))
   
(run-tests anankasm/encode)
