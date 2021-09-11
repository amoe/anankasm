#lang racket/base

(require racket/system)
(require racket/format)
(require racket/file)
(require racket/match)
(require "util.rkt")

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

(define (rip unique-id #:ripper [ripper 'morituri])
  (clean-previous-rip unique-id)

  (let ((full-path (get-rip-path unique-id)))
    (let ((full-command (determine-rip-command ripper full-path)))
      (system/checked full-command))))

(define (determine-rip-command ripper full-path)
  (match ripper
    ['morituri
     (format "rip cd rip -o 6 -U -O '~a' --track-template='%t' --disc-template='' --profile=wav" 
             full-path)]
    ['cdparanoia
     (format "mkdir ~a; cd ~a; cdparanoia -z -O 6 -B; rename -e 's/^track//;' -e 's/\\.cdda\\.wav/.wav/;' *.wav" 
             full-path full-path)]
    ['whipper
     ;; Whipper will auto-rip to FLAC after <https://github.com/whipper-team/whipper/pull/121>
     ;; Needs fixing in anankasm.
     (format "whipper cd rip -o 6 -U -O '~a' --track-template='%t' --disc-template=''"
             full-path)]
    [_ (raise-argument-error 'determine-rip-command "valid ripper" ripper)]))
      
			      
(define (clean-previous-rip unique-id)
  (let ((path (get-rip-path unique-id)))
    (when (directory-exists? path)    ; XXX: race condition
      (delete-directory/files (get-rip-path unique-id)))))

(define (get-rip-path unique-id)
  (build-path (find-system-path 'home-dir) ".anankasm" "rip" (~a unique-id)))
