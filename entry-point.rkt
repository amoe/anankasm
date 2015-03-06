#lang racket

(require "rip.rkt")
(require "encode.rkt")

(match (current-command-line-arguments)
  ((vector output)
   (rip "FOOBAR")
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output))
  (else
   (error 'something "die")))
   

