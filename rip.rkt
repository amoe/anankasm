#lang racket/base

(require racket/system)
(require racket/format)
(require racket/file)

(provide rip
	 clean-previous-rip)

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
  (let ((full-path (get-rip-path unique-id)))
    (let ((full-command (format "rip cd rip -o 6 -U -O '~a' --track-template='%t' --disc-template='' --profile=wav" full-path)))
      (system/exit-code full-command))))
      
			      
(define (clean-previous-rip unique-id)
  (delete-directory/files (get-rip-path unique-id)))

(define (get-rip-path unique-id)
  (build-path (find-system-path 'home-dir) ".anankasm" "rip" (~a unique-id)))
