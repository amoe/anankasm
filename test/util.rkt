#lang racket/base

(require racket/sequence
	 racket/path
	 racket/string
	 racket/system
	 racket/port
	 racket/match
	 racket/list
	 "../util.rkt")

; These are second order level test utilities and do not themselves have tests

(provide get-track-count-from-cd-toc
	 find-bit-rate
	 find-sample-rate
	 is-valid-flac?
	 get-flacs-in-directory
	 get-wavs-in-directory
	 basename
	 valid-wav?
	 get-wav-duration
	 get-track-durations-from-cd-toc
	 second->sector
	 generate-tones)

(define (generate-tones number
			output-directory
			#:sample-rate (sample-rate 44100)
			#:bit-rate (bit-rate 16))
  (for-each (lambda (path)
	      (generate-tone path sample-rate bit-rate))
	    (map (lambda (n)
		   (build-path output-directory (format "~a.wav" n)))
		 (build-list number add1))))
  

(define (generate-tone output-path sample-rate bit-rate)
 (checked-system (format "sox -n -r ~a -b ~a ~a synth 5 sin 440"
			 sample-rate
			 bit-rate
			 output-path)))

(define (is-valid-flac? file)
  (match (system-with-output-and-exit-code (format "flac -t ~a" file))
    ((list output exit-code)
      (= exit-code 0))))

(define (find-bit-rate file)
 (string->number (checked-system (format "soxi -b ~a" file))))

(define (find-sample-rate file)
 (string->number (checked-system (format "soxi -r ~a" file))))

(define (checked-system shell-fragment)
  (match (system-with-output-and-exit-code shell-fragment)
    ((list output exit-code)
     (when (not (zero? exit-code))
       (error 'checked-system
	      "subprocess ~s failed with code ~a: ~s"
	      shell-fragment
	      exit-code
	      output))
     (string-trim output "\n" #:left? #f))))


(define (get-track-count-from-cd-toc)
  ; Call cdparanoia and break the output into lines and grep it.
  (match (system-with-output-and-exit-code (format "cdparanoia -Q 2>&1"))
    ((list output exit-code)
     (when (not (zero? exit-code))
       (error 'get-track-count-from-cd-toc
	      "subprocess failed: ~s" output))
     (count is-track-line? (string-split output (string #\newline))))))

(define (get-flacs-in-directory path)
  (sort (sequence->list (sequence-filter is-flac? (in-directory path))) path<?))

(define (get-wavs-in-directory path)
  (sort (sequence->list (sequence-filter is-wav? (in-directory path))) path<?))

(define (valid-wav? path)
  (match (system-with-output-and-exit-code (format "soxi -t ~a" path))
    ((list output exit-code)
     (and
      (string=? (string-trim output) "wav")
      (= exit-code 0)))))

(define (get-wav-duration path)
  (match (system-with-output-and-exit-code (format "soxi -D ~a" path))
    ((list output exit-code)
     (when (not (zero? exit-code))
       (error 'get-wav-duration
	      "subprocess failed: ~a" path))

     (string->number (string-trim output)))))

(define (get-track-durations-from-cd-toc)
  (map sector->second
       (match (system-with-output-and-exit-code (format "cdparanoia -Q 2>&1"))
	 ((list output exit-code)
	  (when (not (zero? exit-code))
	    (error 'get-track-durations-from-cd-toc
		   "subprocess failed: ~s" output))
	  
	  (map
	   (lambda (line)
	     (match (regexp-match #px"\\s*\\d+\\.\\s*(\\d+) .*" line)
	       ((list full sector-duration)
		(string->number sector-duration))))
	   (filter is-track-line?
		   (string-split output (string #\newline))))))))

(define (system-with-output-and-exit-code str)
    (match (process str)
    ((list standard-output
	   standard-input
	   pid
	   standard-error
	   control)
     (close-output-port standard-input)
     (port->string standard-error)
     (close-input-port standard-error)
     (control 'wait)
     (let ((output (port->string standard-output)))
       (close-input-port standard-output)
       (list output (control 'exit-code))))))


(define (is-track-line? line)
  (regexp-match? #px"^\\s*\\d+\\." line))


(define (sector->second n)
  (/ n 75))

(define (second->sector n)
  (* n 75))

(define (is-wav? path)
  (bytes=? (filename-extension path) #"wav"))

(define (is-flac? path)
  (bytes=? (filename-extension path) #"flac"))
