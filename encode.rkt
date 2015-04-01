#lang racket/base

(require racket/file
	 racket/sequence
	 racket/system)

(require "util.rkt")

(provide encode
	 (struct-out encode-format)
	 default-encode-format
	 find-encoder)

(struct encode-format (name encode-command extension))

(define formats
  (list
   (encode-format 'flac "flac --best -o ~a ~a" "flac")
   (encode-format 'ogg "oggenc -q 4 -o ~a ~a" "ogg")))

(define (find-encoder name)
  (findf (lambda (fmt) (eq? (encode-format-name fmt) name))
	 formats))

(define default-encode-format (make-parameter (find-encoder 'ogg)))

; Expect a path representing a directory containing WAV files.
; Encodes all files to FLAC.
; Will also fix files that need to be downsampled.
; Requires sox.

; So where do we expect the output to appear?
(define (encode input-directory output-directory
                #:filter [wave? (make-extension-filter "wav")])
  (printf "inside encode function\n")
  (let ((fixed-output (make-temporary-file "fixed-~a" 'directory)))
    (fix-waves input-directory fixed-output wave?)
    (flacize-files fixed-output output-directory)))

(define (fix-waves input-directory output-directory wave?)
  (for-each
   (lambda (path)
     (printf "considering ~s\n" path)
     (fix-wave path (build-path output-directory
				; This is kind of dumb as we unnecessarily
				; remove and cat back on the extension.
				(format "~a.wav" (basename path)))))
     (filter wave?
	     (sequence->list (in-directory input-directory)))))

(define (fix-wave input-path output-path)
  (let ((result (system/exit-code
		 (format "sox -G ~a -r 44100 -c 2 -e signed-integer -b 16 ~a"
			 (shell-quote (path->string input-path))
			 (shell-quote (path->string output-path))))))
    (when (not (zero? result))
      (error 'fix-wave "conversion subprocess failed with code ~a" result))))

(define (flacize-files input-directory output-directory)
  (for-each
   (lambda (path)
     (flacize-single-file path (build-path output-directory
				(format "~a.~a"
					(basename path)
					(encode-format-extension (default-encode-format))))))
     (sequence->list (in-directory input-directory))))

(define (flacize-single-file input-file output-file)
  (let ((command (format (encode-format-encode-command (default-encode-format))
			 (shell-quote (path->string output-file))
			 (shell-quote (path->string input-file)))))

    (let ((result (system/exit-code command)))
      (when (not (zero? result))
	(error 'fix-wave "encode subprocess ~a failed with code ~a"
	       command result)))))

