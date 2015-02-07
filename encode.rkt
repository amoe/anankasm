#lang racket/base

(provide encode)

; Expect a path representing a directory containing WAV files.
; Encodes all files to FLAC.
; Will also fix files that need to be downsampled.
; Requires sox.

; So where do we expect the output to appear?
(define (encode input-directory output-directory)
  #t)
