#lang scheme

(require scheme/system)
(require srfi/1)

; Assume the first line is the index.

(define rg-map
  '(("dB gain" . gain)
    ("Max Amplitude" . peak)))

(define (analyze files)
  (let ((data (map (lambda (line) (regexp-split "\t" line))
		      (run-mp3gain files))))
    (let ((header (first data))
	  (records (rest data)))
      (let-values (((tracks album)
		    (split-at records
			      (- (length records) 1))))
        ;(values header tracks (car album))))))
        (scan-header header)))))
					     
(define (scan-header header)
  ; Test each value in HEADER to see if it string=? a value in rg-map.
  ; If so, cons a pair with '(CURRENT-LIST-INDEX . VALUE-OF-RG-MAP-KEY)
  (let loop ((lst header) (idx 0))
    (cond
      ((null? lst)  '())
      ((assoc (car lst) rg-map)
        => (lambda (pair)
	     (cons
	       (cons idx (cdr pair))
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
