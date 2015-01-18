#lang racket/base

(require rackunit
	 racket/sequence
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
 "Tracks are named sequentially"
 ; Sort directory list, generate integer sequence, iterate in parallel,
 ; terminate on every.
 (rip)
 (let ((expected-list (build-list (util:get-track-count-from-cd-toc)
				  add1)))
   (check-true (andmap (lambda (path n)
			 (= (util:basename path) n))
		       (sort (directory-list expected-output-path) path<?)
		       expected-list))))


(test-case
 "Tracks are in valid WAV format"
 (rip)
 (sequence-andmap util:valid-wav? (in-directory expected-output-path)))

(test-case
 "Track times match CD TOC"
 (let ((normalize-second (compose round util:second->sector)))
   (check-equal? 
    (sequence->list
     (sequence-map (compose normalize-second
			    util:get-wav-duration)
		   (in-directory expected-output-path)))
    (map normalize-second util:get-track-durations-from-cd-toc))))
  
