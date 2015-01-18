#lang racket/base

(require rackunit "../rip.rkt")

(test-case
 "Output directory was created"
 (let* ((unique-id "FROM-TEST-SUITE")
	(output-path (build-path "/home/amoe/.anankasm/rip"
				 unique-id)))
   (rip unique-id)
   (directory-exists? output-path)))


(test-case
 "Track count matches TOC"
 (fail))

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
