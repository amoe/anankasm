#lang scheme

(require scheme/system)
(require srfi/1)
(require srfi/26)

(require "interface.scm")
(require "options.scm")

;(define f0 (let ((d "/home/amoe/mortville")) (map path->string (map (lambda (x) (build-path d x)) (directory-list d)))))
(provide replaygain)

; The RG-MAP - maps database keys to a pair (TAG-SUFFIX, TRANSFORM-PROC).
; When the key is recognized, TRANSFORM-PROC is applied to its value to
; yield the value of TAG-SUFFIX.
; This is a procedure, rather than a constant, because FORMAT-* are not defined
; until later in the module - they can't be eagerly evaluated now.
(define (rg-map)
  (quasiquote (("dB gain"       . (gain . (unquote format-gain)))
	       ("Max Amplitude" . (peak . (unquote format-peak))))))

; Replaygain algorithm mk2
(define (replaygain files)
  (let-values (((header tracks album)
		(analyze files)))
    (let ((rg-index (scan-header header)))
      (for-each apply-text-tags
	(map
	  (possibly (not (option 'mastering-disparity))
		    (cute append <> (album-tags album rg-index)))
	  (map (cute path->track-tags tracks rg-index <>)
	       files))
	files))))

(define (possibly pred? proc)
  (if pred?
      (lambda (x) (proc x))
      (lambda (x) x)))

; Bit random, but basically this converts a universal (generic) tag like
; 'gain, which is a symbol, to a specific tag in a context like
; "REPLAYGAIN_TRACK_GAIN".  The PREFIX designates this context.
; More generally, this formats the tag from the raw MP3gain data
; to match the proper RG format as dictated by vorbisgain for the moment.)))
; That's 8 decimal places on the peak value which is divided by 32768,
; and whack a " dB" on the end of the gain values, rounding them off to 2
; decimal places.
; Example:
;
;REPLAYGAIN_TRACK_PEAK=1.13956976
;REPLAYGAIN_TRACK_GAIN=-5.74 dB
;REPLAYGAIN_ALBUM_PEAK=1.18345642
;REPLAYGAIN_ALBUM_GAIN=-6.63 dB
(define (tag:particularize tag prefix)
  (cons
    (string-append prefix (symbol->string (car tag)))
    (cdr tag)))

(define (format-gain x)
  (let ((n (string->number x)))
    (format "~a~a dB"
	    (if (positive? n) "+" "")        ; The write form of a negative
	    (real->decimal-string n 2))))    ; number already includes a minus

(define (format-peak x)
  (real->decimal-string (/ (string->number x) 32768) 8))
  

(define (album-tags album rg-index)
  (map (cute tag:particularize <> "replaygain_album_")
       (get-rg-from-record album rg-index)))

(define (path->track-tags tracks rg-index path)
  (let ((record (assoc path tracks)))
    (if record
	(map (cute tag:particularize <> "replaygain_track_")
	     (get-rg-from-record record rg-index))
	(error 'get-rg-for-path
	       "mp3gain gave no replaygain value for path ~s"
	       path))))
	
(define (get-rg-from-record record rg-index)
  (map (lambda (pair)
	 (cons (car pair)
	       ((cdr pair) record)))
       rg-index))

(define (analyze files)
  (let ((data (map (lambda (line) (regexp-split "\t" line))
		      (run-mp3gain files))))
    (let ((header (first data))
	  (records (rest data)))
      (let-values (((tracks album)
		    (split-at records
			      (- (length records) 1))))
        (values header tracks (car album))))))
					     
; Test each value in HEADER to see if it string=? a value in rg-map.
; If so, cons a pair with '(CURRENT-LIST-INDEX . RETRIEVE-PROCEDURE).
; RETRIEVE-PROCEDURE can then be called on a record to get the desired
; value, with context-sensitive modifications.
(define (scan-header header)
  (let loop ((lst header) (idx 0))
    (cond
      ((null? lst)  '())
      ((assoc (car lst) (rg-map))
        => (lambda (pair)
	     (cons
	       (cons (cadr pair)
		     (compose (cddr pair)
			      (cute list-ref <> idx)))
	       (loop (cdr lst) (+ idx 1)))))
      (else (loop (cdr lst) (+ idx 1))))))
    
; NB: It's possible that this suffers from that nasty bug of old.
(define (run-mp3gain files)
  (let ((l (apply process*
		  (append (list "/usr/bin/mp3gain" "-s" "s" "-o")
			  files))))
    (slurp-lines (first l))))

(define (apply-text-tags tags file)
  (for-each
   (lambda (tag)
     (let ((str (format "--set-user-text-frame=~a:~a"
                        (car tag) (cdr tag))))
       (apply run-command (list *default-eyed3* str file))))
   tags))
