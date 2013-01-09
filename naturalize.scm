#lang scheme

(require scheme/string)
(require scheme/system)

(require srfi/1)   ; list library
(require srfi/26)  ; cut & cute
(require srfi/64)

(require (prefix-in taglib:    "taglib.scm"))
(require (prefix-in munge-tag: "munge-tag.scm"))
(require "histogram.scm")
(require "interface.scm")
(require "options.scm")
(require "replaygain.scm")

(provide main)

(define *tag-map*
  (list
   (cons 'artist (cons taglib:tag-artist taglib:tag-set-artist))
   (cons 'album (cons taglib:tag-album taglib:tag-set-album))
   (cons 'tracknumber (cons taglib:tag-track taglib:tag-set-track))
   (cons 'title (cons taglib:tag-title taglib:tag-set-title))
   (cons 'date (cons taglib:tag-year taglib:tag-set-year))
   (cons 'genre (cons taglib:tag-genre taglib:tag-set-genre))))

; ENTRY POINT
(define (main . args)
  (check-commands)
  
  (let ((args (configure args)))
    (check-permissions args)

    (let ((times (save-mtimes args)))
      (define (cleanup v)  (load-mtimes args times))

      (with-handlers ((exn:break? cleanup))
        (let ((tmpl (pass-to-editor (apply files->template args))))
          (strip-tags args)
          (apply-tags tmpl args)

	  (display "applying replaygain... ")
	  (flush-output)
          (unless (option 'skip-replaygain)
            (replaygain args))
	  (say "done.")
        
          (load-mtimes args times)
          (move-files tmpl args))))))

(define (check-permissions lst)
  (when (not (every writable? lst))
    (error 'check-permissions
           "not all files are writable, please fix and rerun")))

(define (check-commands)
  (let loop ((required-executables '("mp3gain" "vorbisgain" "eyeD3")))
    (when (not (null? required-executables))
      (when (not (find-executable-path (car required-executables)))
        (error 'check-commands "command not found"))
      (loop (cdr required-executables)))))

(define (files->template . args)
  
  (define tags
    (map
     (lambda (tag)
       (cons tag
             (apply select-from-tags
                    (cons (lookup-getter tag) args))))
     (global-tag-list)))

     
  (define tracks
    (let ((track-indices (iota (length args) 1)))
      (map
       (lambda (file index)
         (map (lambda (tag)
                (if (and (eq? tag 'tracknumber)
                         (option 'number-automatically))
                    index
                    (tag-proc/cleanup (lookup-getter tag) file)))
              (local-tag-list)))
       args track-indices)))

  (cons tags tracks))



(define (strip-tags args)
  (debug "stripping tags")
  (apply run-command
         (cons *default-eyed3*
               (cons "--remove-all"
                     (cons "--no-color" args)))))

(define (apply-tags tmpl files)
  (say "writing tags...")
  (for-each
   (lambda (t) (apply-one-tag t files))
   (template:global-tags tmpl))

  (apply-local-tags (template:local-tags tmpl) files))

(define (move-files tmpl files)
  (debug "moving files to correct locations")
  (for-each
   (lambda (old new)
     (make-parents new)
     
     (with-handlers ((exn:fail:filesystem:exists? (const #t)))
       (rename-file-or-directory old new)))    ; FIXME: only attempt move if !=
   files (template->new-names tmpl files)))

; Make the parents of PATH
(define (make-parents path)
  (let-values (((base name must-be-dir?)
                (split-path path)))
    (make-directory* base)))

(define (identity x) x)

(define *abbreviated-tag-map*
  (list
    (cons "a" (cons 'artist      identity))
    (cons "A" (cons 'album       identity))
    (cons "t" (cons 'title       identity))
    (cons "T" (cons 'tracknumber (compose (cute zero-pad <> 2) number->string)))
    (cons "d" (cons 'date        number->string))
    (cons "g" (cons 'genre       identity))))

(define (zero-pad str len)
  (let ((diff (- len (string-length str))))
    (if (positive? diff)
        (string-append (make-string diff #\0) str)
        str)))

; We can only build part of the list at the start so this is not valid
(define (template->new-names tmpl files)
  (let ((ga (build-global-abbrevs tmpl)))
    (map
      (lambda (lt file)
        (let ((la (build-local-abbrevs lt)))
          (append-extension
           (xformat (filename-template)
                    (append ga la))
           file)))
      (template:local-tags tmpl) files)))

; Append the extension of old to new, if there was one
(define (append-extension new old)
  (let ((ext (filename-extension old)))
    (if ext
        (string-append new "." (bytes->string/locale ext))
        old)))
    
(define (build-local-abbrevs lt)
  (filter-map
    (lambda (abbrev)
      (let ((proc (cddr abbrev)))
        (cond
         ((list-index (cute eq? <> (cadr abbrev)) (local-tag-list))
           => (lambda (idx)
                (cons (car abbrev)
                      (munge-tag:munge-tag
                        (proc (list-ref lt idx))))))
         (else #f))))
    *abbreviated-tag-map*))

(define (build-global-abbrevs tmpl)
  (let ((gt (template:global-tags tmpl)))
    (map
      (lambda (abbrev)
        (let ((proc (cddr abbrev)))
          (cons
            (car abbrev)
            (munge-tag:munge-tag
              (proc (cdr (assq (cadr abbrev) gt)))))))
      (filter
        (lambda (abbrev)
          (memq (cadr abbrev) (global-tag-list)))
        *abbreviated-tag-map*))))

; At the moment we're not displaying the histogram frequencies to the user,
; we're just sorting by them, so option 1 always has the highest frequency
(define (select-from-tags get-tag . args)
  (let ((tags (map (lambda (f) (tag-proc/cleanup get-tag f)) args)))
    (let ((hist (sort (histogram tags) frequency>?)))
      (if (just-one? hist)
          (caar hist)
          (choose (map car hist) 1)))))

(define (lookup-getter tag)
  (cadr (assq tag *tag-map*)))

(define (lookup-setter tag)
  (cddr (assq tag *tag-map*)))


(define (save-mtimes files)
  (map file-or-directory-modify-seconds files))

(define (load-mtimes files times)
  (for-each file-or-directory-modify-seconds files times))

(define (just-one? lst) (= (length lst) 1))

(define (tag-proc/cleanup proc file)
  (let ((f (taglib:file-new file)))
    (let ((datum (proc (taglib:file-tag f))))
      (taglib:file-save f)
      (taglib:file-free f)
      datum)))

(define template:global-tags car)
(define template:local-tags  cdr)

(define (apply-local-tags lt files)
  (for-each
    (lambda (local-tag file)
      (for-each
        (lambda (key val)
          (tag-proc/cleanup
           (lambda (t)
             ((lookup-setter key) t val))
           file))
        (local-tag-list) local-tag))
    lt files))

(define (apply-one-tag tag files)
  (for-each
   (lambda (file)
    (tag-proc/cleanup
     (let ((set-tag (lookup-setter (car tag))))
       (lambda (t) (set-tag t (cdr tag))))
     file))
   files))

