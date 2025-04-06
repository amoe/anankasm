#lang racket

(require "rip.rkt")
(require "encode.rkt")
(require "util.rkt")
(require (prefix-in naturalize: "naturalize.scm"))

(define (is-encodable? path)
  (printf "considering ~s in wave?\n" path)
  (let ((wav? (make-extension-filter "wav"))
	(flac? (make-extension-filter "flac")))
    (or (wav? path) (flac? path))))

(match (current-command-line-arguments)
  ;; Rips with cdparanoia
  ((vector "rip" output)
   (rip "FOOBAR")
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output)
   (delete-directory/files "/home/amoe/.anankasm/rip/FOOBAR"))

  ((vector "rip-whipper" output)
   (rip "FOOBAR" #:ripper 'whipper)
   (encode "/home/amoe/.anankasm/rip/FOOBAR" output)
   (delete-directory/files "/home/amoe/.anankasm/rip/FOOBAR"))

  ((vector "encode" flac-dir output-dir)
   (printf "Encoding\n")
   (encode flac-dir output-dir #:filter is-encodable?))
  ((vector "tag" rest-of-stuff ...)
   (printf "about to naturalize\n")
   (apply naturalize:main rest-of-stuff)
   (printf "finished naturalizing\n"))
  (else
   (error 'entry-point "unable to understand desired action")))
   

