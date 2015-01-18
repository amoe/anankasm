#lang racket/base

(require rackunit
	 "../rip.rkt"
	 (prefix-in util: "util.rkt"))

(define unique-id "FROM-TEST-SUITE")
(define expected-output-path (build-path "/home/amoe/.anankasm/rip" unique-id))

(test-case
 "Output directory was created"
 (rip unique-id)
 (directory-exists? expected-output-path))

(test-case
 "Track count matches TOC"
 (let ((track-count (util:get-track-count-from-cd-toc)))
   (rip unique-id)
   (check-equal? (util:count-files-in-directory expected-output-path)
		 track-count)))

(test-case
 "Directory name uses unique value"
 (fail))

(test-case
 "Tracks are named sequentially"
 (fail))


(test-case
 "Tracks are in valid WAV format"
 (fail))

(test-case
 "Track times match CD TOC"
 (fail))
