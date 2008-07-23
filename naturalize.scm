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

(provide main)

; *** BUGS: ****
; - Mangles unicode tags, do not know why
; The bug is TAGLIB - 1.5 added unicode selection.  Before this, values
; get mangled, do not know why.
; Please use TL 1.5 or greater.
; - Replaygain causes files to be silent.
;     It is apply-text-tags function that causes it.
;     Tracks created as such are silent in rhythmbox.
;     They decode with madplay to a WAV that will then play in
;     any app, however peak level for madplay is always 0.
;     xxd shows that all content is still present.
;     So far, this happens on VBR and CBR 320kbps MP3s
;     MP3 doesn't matter.  The second you add these tags, it breaks RB.
;     Switching tracks & removing from the database and readding doesn't fix
;     either.  It's required to restart RB and re-add the file after
;     removing the tags.   - only sometimes.
;     Adding just track tags doesn't break MP3s.
;       -- Actually, yes it does.  Adding only album tags breaks too.
;  CONCLUSION:
;    This looks an awful lot like a bug in RB.
;    Unfortunately it ain't easy to test, but Audacious eats the generated
;    mp3s fine, and madplay decodes them fine too.
;    I believe rockbox plays them too, since that solid paranoid crap worked.
;  FREAKY THING:
;   Boom MP3s still work!  Even tho they have tags!  And they STILL WORK
;   FINE, every single one.  MADNESS.


; orange JUICE
; orange JUICE
; orange JUICE
; for life!


; Release BLOCKERS:
; Errors need to be improved: no input provided, non-mp3 provided, etc.


(define *default-eyed3*    "/usr/bin/eyeD3")
(define *default-mp3gain*  "/usr/bin/mp3gain")

(define *tag-map*
  (list
   (cons 'artist (cons taglib:tag-artist taglib:tag-set-artist))
   (cons 'album (cons taglib:tag-album taglib:tag-set-album))
   (cons 'tracknumber (cons taglib:tag-track taglib:tag-set-track))
   (cons 'title (cons taglib:tag-title taglib:tag-set-title))
   (cons 'date (cons taglib:tag-year taglib:tag-set-year))
   (cons 'genre (cons taglib:tag-genre taglib:tag-set-genre))))

; ENTRY POINT
; note: Because of the way tag-proc/cleanup is imp'd, files->template
; WRITES files, thus mutilating their mtime.  Should be fixed, but
; this workaround is fine.
(define (main . args)
  (let ((args (configure args)))
    (let ((times (save-mtimes args)))
      (let ((tmpl (pass-to-editor (apply files->template args))))
        (strip-tags args)
        (apply-tags tmpl args)
        (replaygain args)
        
        (load-mtimes args times)
        (move-files tmpl args)))))

(define (files->template . args)
  (define tags
    (map
     (lambda (tag)
       (cons tag
             (apply select-from-tags
                    (cons (lookup-getter tag) args))))
     (global-tag-list)))
     
  (define tracks
    (map
     (lambda (file)
       (map
        (lambda (tag)
          (tag-proc/cleanup (lookup-getter tag) file))
        (local-tag-list)))
     args))

  (cons tags tracks))



(define (strip-tags args)
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

; See link below for the source of this algorithm.
; http://www.hydrogenaudio.org/forums/index.php?showtopic=42005
; Please make sure you have write permissions on files.
(define (replaygain files)
  ; igoldgain algorithm
  ; first, copy all files
  (display "calculating replaygain values... ")
  (flush-output)

  (let ((tmp (map
               (cute make-temporary-file "naturalize-~a.mp3" <>)
               files))
        (options (list "-s" "r" "-c" "-a")))
    (apply run-command
           (cons *default-mp3gain*
                 (append options (map path->string tmp))))
    (say "done.")

     (for-each
       (lambda (orig copy)
         (apply-text-tags (gain-info-from-tags copy)
                          orig))
     
       files (map path->string tmp))
    
    (for-each delete-file tmp)))

; Need to create all preceding folders before being able to move here.
(define (move-files tmpl files)
  (for-each
   (lambda (old new)
     (make-parents new)
     (rename-file-or-directory old new))
   files (template->new-names tmpl files)))


; WARNING: DARK DARK MAGIC
(define (make-parents path)
  (for-each make-directory/uncaring
    (map (cute apply build-path <>)
         (map reverse
              (unfold-right
                null?
                identity
                cdr
                (cdr (reverse (explode-path path))))))))

; warning: race condition
(define (make-directory/uncaring path)
  (when (not (directory-exists? path))
    (make-directory path)))

; Alternative way to do this:
;  Define mapping from abbrev to full tag,
;  For each abbrev, lookup the tag in the global and local tag lists,
;  If in the global list, assq it;
;  If in the local list, list-ref it based the position in local tag list.
;  Some tags need special processing, like zero-padding on the tracknumber.
;  So the mapping can look like:
;  ((abbreviation . (full-tag-name . preprocessor)))
;  Where the preprocessor is the identity function normally, and is the
;  appropriate conversion function otherwise.
;  Each tag is always put through the munger, regardless of the preprocessor

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
        

(define (gain-info-from-tags file)
  (let ((l (apply process* (list *default-mp3gain* "-s" "c" file))))
    (let ((output (slurp-lines (first l))))

    (close-input-port (first l))
    (close-output-port (second l))
    (close-input-port (fourth l))
    
    (filter-map rg-tag output))))

; Return: #f if no match, or the appropriate pair if match
; FILTER-MAP this function across the returned list to get the correct stuff

(define (rg-tag line)
  (any
    (lambda (x)
      (if (regexp-match? (car x) line)
          (cons (cdr x) (after-colon line))
          #f))
    (rg-lines)))

(define (apply-text-tags tags file)
  (for-each
   (lambda (tag)
     (let ((str (format "--set-user-text-frame=~a:~a"
                        (car tag) (cdr tag))))
       (apply run-command (list *default-eyed3* str file))))
   tags))

(define (after-colon str)
  (let ((x (regexp-match #px".*:\\s*([-\\.\\d]+)" str)))
    (if x
      (cadr x)
      (error "internal fuckup: invalid string passed to after-colon"))))

(define (slurp-lines port)
  (let ((l (read-line port)))
    (if (eof-object? l)
        '()
        (cons l (slurp-lines port)))))

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
