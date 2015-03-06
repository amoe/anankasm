#lang racket

(require "rip.rkt")
(require "encode.rkt")

(match (current-command-line-arguments)
  ((vector output)
   (rip "FOOBAR")
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output)
   (delete-directory/files "/home/amoe/.anankasm/rip/FOOBAR"))
  (else
   (error 'something "die")))
   

