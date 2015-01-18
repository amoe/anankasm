#lang racket/base

(require racket/system)

(provide rip)

; After rip finishes, a file should have been created under the backup
; destination.  It should contain the same number of tracks.
; Name it after the epoch time.
; The global settings object...
; How can we handle this?
; We don't want to have to pass it to every function.
; Oh, we use parameters.
; But we strive to make every function.
; The tracks should have the correct names.
; The tracks should be in WAV format.
; The track times should match the timestamp specified by the CD TOC.
(define (rip unique-id)
  #f)
				
