#lang racket/base

(require racket/sequence
	 racket/path
	 racket/string
	 racket/system
	 racket/port
	 racket/match
	 racket/list
	 racket/function
	 racket/format
	 "util.rkt")

; These are second order level test utilities and do not themselves have tests

(provide get-track-count-from-cd-toc
	 find-bit-rate
	 find-sample-rate
	 is-valid-encode?
	 basename
	 valid-wav?
	 get-wav-duration
	 get-track-durations-from-cd-toc
	 get-file-type-in-directory
	 second->sector
	 generate-tones
	 make-dummy-wave
	 make-cue-sheet
	 load-dummy-cue-sheet
	 clean-dummy-wave
	 clean-cue-sheet
	 unload-dummy-cue-sheet)


(define (make-cue-sheet)
  (with-output-to-file "mytoc.cue"
    (thunk (display (generate-cue-sheet)))
    #:exists 'truncate))

(define (make-dummy-wave)
  (let ((seconds-value (* 8 60)))
    (system/checked
     (format
      "sox -n -c 2 -r 44100 -e signed-int -b 16 blah.wav trim 0.0 ~a"
      seconds-value))))


(define (load-dummy-cue-sheet)
  (system/checked "cdemu load 0 mytoc.cue"))

(define (clean-dummy-wave)
  (delete-file "blah.wav"))

(define (clean-cue-sheet)
  (delete-file "mytoc.cue"))

(define (unload-dummy-cue-sheet)
  (system/checked "cdemu unload 0"))


(define (generate-cue-sheet)
  (let ((sep (string #\newline))
	(n-tracks 8))
    (string-join (map (lambda (n) (generate-track-statements n (- n 1)))
		      (sequence->list (in-range 1 (+ n-tracks 1))))
		 sep
		 #:after-last sep
		 #:before-first "FILE \"blah.wav\" WAVE\n")))

(define (zero-pad n width)
  (~a n
      #:left-pad-string "0"
      #:min-width width
      #:align 'right))

(define (generate-track-statements n ts)
  (string-join 
   (list
    (format "TRACK ~a AUDIO" (zero-pad n 2))
    (format "INDEX 01 ~a:00:00" (zero-pad ts 2)))
   (string #\newline)))



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

(define (choose-test-command format-id)
  (case format-id
    ((ogg) "oggdec -o /dev/null ~a")
    ((flac) "flac -t ~a")
    (else
     (error 'choose-test-command "cannot test unknown format" format-id))))

(define (is-valid-encode? file format-id)
  (printf "~s\n" file)
  (printf "~s\n" format-id)

  (match (system-with-output-and-exit-code (format (choose-test-command 
						    format-id) file))
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

(define (get-file-type-in-directory path is-extension?)
  (sort
   (sequence->list 
    (sequence-filter is-extension? (in-directory path))) path<?))

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

