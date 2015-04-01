#lang racket

(require "rip.rkt")
(require "encode.rkt")
(require (prefix-in naturalize: "naturalize.scm"))

(match (current-command-line-arguments)
  ((vector "rip" output)
   (rip "FOOBAR")
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output)
   (delete-directory/files "/home/amoe/.anankasm/rip/FOOBAR"))
  ((vector "tag" rest-of-stuff ...)
   (apply naturalize:main rest-of-stuff))
  (else
   (error 'entry-point "unable to understand desired action")))
   

