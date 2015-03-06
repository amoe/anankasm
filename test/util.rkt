#lang racket/base

(require rackunit
	 rackunit/text-ui
	 "../util.rkt"
	 (prefix-in test-util: "../test-utility.rkt"))

; Test suite for utility procedures

(define-test-suite anankasm/util
  (test-case
   "basename has the correct output"
   (check-equal? (basename (string->path "foo.txt")) "foo")))

(run-tests anankasm/util)

