#lang racket

(require "encode.rkt")

(match (current-command-line-arguments)
  ((vector input output)
   (encode input output)) 
  (else
   (error 'something "die")))
   

