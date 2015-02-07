#lang racket/base

(require racket/file
	 racket/sequence
	 racket/system)

(require "util.rkt")

(provide encode)

; Expect a path representing a directory containing WAV files.
; Encodes all files to FLAC.
; Will also fix files that need to be downsampled.
; Requires sox.

; So where do we expect the output to appear?
(define (encode input-directory output-directory)
  (let ((fixed-output (make-temporary-file "fixed-~a" 'directory)))
    (fix-waves input-directory fixed-output)
    (flacize-files fixed-output output-directory)))

(define (fix-waves input-directory output-directory)
  (for-each
   (lambda (path)
     (fix-wave path (build-path output-directory
				; This is kind of dumb as we unnecessarily
				; remove and cat back on the extension.
				(format "~a.wav" (basename path)))))
     (sequence->list (in-directory input-directory))))

(define (fix-wave input-path output-path)
  (let ((result (system/exit-code
		 (format "sox -G ~a -r 44100 -c 2 -e signed-integer -b 16 ~a"
			 input-path output-path))))
    (when (not (zero? result))
      (error 'fix-wave "conversion subprocess failed with code ~a" result))))

(define (flacize-files input-directory output-directory)
  (for-each
   (lambda (path)
     (flacize-single-file path (build-path output-directory
				(format "~a.flac"
					(basename path)))))
     (sequence->list (in-directory input-directory))))

(define (flacize-single-file input-file output-file)
  (let ((result (system/exit-code
		 (format "flac --best -o ~a ~a" output-file input-file))))
    (when (not (zero? result))
      (error 'fix-wave "encode subprocess failed with code ~a" result))))

