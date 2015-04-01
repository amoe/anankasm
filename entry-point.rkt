#lang racket

(require "rip.rkt")
(require "encode.rkt")
(require "naturalize.scm")

(match (current-command-line-arguments)
  ((vector "rip" output)
   (rip "FOOBAR")
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output)
   (delete-directory/files "/home/amoe/.anankasm/rip/FOOBAR"))
  ((vector "tag" input)


   (apply main (sequence->list (in-directory input))))
  (else
   (error 'something "die")))
   

