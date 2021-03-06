; This module holds options and constants that reconfigure themselves
; based on said options

#lang racket

(require racket/cmdline)
(require srfi/1)
(require srfi/78)

(provide
 global-tag-list
 local-tag-list
 filename-template
 configure
 option
 option!)

(define *options*
  '((va-mode             . #f)
    (mastering-disparity  . #f)
    (preserve-mtimes      . #t)
    (debug                . #t)
    (verbose              . #f)
    (number-automatically . #f)
    (skip-replaygain . #f)))

(define (configure args)
  (command-line
   #:program "naturalize"
   #:argv args
   #:once-each
   (("-V" "--va-mode")             "Various Artists mode"
    (option! 'va-mode #t))
   (("-v" "--verbose")             "show output from subprocesses"
    (option! 'verbose #t))
   (("-M" "--mastering-disparity") "do not scan album gain"
    (option! 'mastering-disparity #t))
   (("-P" "--mangle-mtimes")       "don't preserve file mtimes" #t)
   (("-n" "--number-automatically") "automatically add track numbers"
    (option! 'number-automatically #t))
   (("-R" "--skip-replaygain") "do not apply replaygain"
    (option! 'skip-replaygain #t))
   #:args (path1 . paths)
   (cons path1 paths)))

(define (global-tag-list)
  (if (option 'va-mode)
      '(album date genre)
      '(artist album date genre)))

(define (local-tag-list)
  (if (option 'va-mode)
      '(tracknumber artist title)
      '(tracknumber title)))

(define (filename-template)
  (if (option 'va-mode)
      "/home/amoe/music/various/%d-%A/%T-%a_-_%t"
      "/home/amoe/music/%a/%d-%A/%T-%t"))

(define (option name)
  (let ((opt (assq name *options*)))
    (if opt
	(cdr opt)
	(error "unknown option"))))

(define (option! name val)
  (set! *options*
	(map
	 (lambda (pair)
	   (let ((key (car pair)))
	     (if (eq? key name)
		 (cons key val)
		 pair)))
	 *options*)))

(define (alist? x)
  (every pair? x))

(define (test)
  (check (alist? *options*) => #t)

  (let ((filenames '("test1.mp3" "test2.mp3")))
    (check (configure filenames) => filenames))

  (check (list? (global-tag-list)) => #t)
  (check (list? (local-tag-list)) => #t)

  (check (string? (filename-template)) => #t)

  (let ((value (option 'va-mode)))
    (check (boolean? value) => #t)
    (check (void? (option! 'va-mode #t)) => #t)
    (check (option 'va-mode) => #t)
    (option! 'va-mode value)))






