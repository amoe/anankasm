#lang scheme

(require scheme/system)
(require srfi/1)
(require srfi/26)

; Interface: APPLY-TEXT-TAGS takes an alist of tags and a path.
; (apply-text-tags tags file)
; Tags are in pair format.

; So, to RG a list FILES, we FOR-EACH over them, extracting the data each
; time.  Each time we need to walk the list to the line matching the data
; for this entry, since there's no guarantee they'll be in order, though
; they always are, since the order of output is not documented.

; Assume the first line is the index.

(define rg-map
  '(("dB gain" . gain)
    ("Max Amplitude" . peak)))

(define (replaygain files)
  (let-values (((header tracks album)
		(analyze files)))
    (let ((rg-index (scan-header header)))
      (for-each
        (lambda (path tags)
	  (printf "path: ~s, tags: ~s\n" path tags))
	files
	(map
	  (cute append <> (album-tags album rg-index))
	  (map (cute path->track-tags tracks rg-index <>)
	       files))))))

; Bit random, but basically this converts a universal (generic) tag like
; 'gain, which is a symbol, to a specific tag in a context like
; "REPLAYGAIN_TRACK_GAIN".  The PREFIX designates this context.
(define (tag:particularize tag prefix)
  (cons
    (string-append prefix (symbol->string (car tag)))
    (cdr tag)))

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
	 (cons
	  (car pair)
	  (list-ref record (cdr pair))))
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
					     
(define (scan-header header)
  ; Test each value in HEADER to see if it string=? a value in rg-map.
  ; If so, cons a pair with '(CURRENT-LIST-INDEX . VALUE-OF-RG-MAP-KEY)
  (let loop ((lst header) (idx 0))
    (cond
      ((null? lst)  '())
      ((assoc (car lst) rg-map)
        => (lambda (pair)
	     (cons
	       (cons (cdr pair) idx)
	       (loop (cdr lst) (+ idx 1)))))
      (else (loop (cdr lst) (+ idx 1))))))
    
(define (run-mp3gain files)
  (let ((l (apply process*
		  (append (list "/usr/bin/mp3gain" "-s" "s" "-o")
			  files))))
    (slurp-lines (first l))))

(define (slurp-lines port)
  (let ((l (read-line port)))
    (if (eof-object? l)
        '()
        (cons l (slurp-lines port)))))
