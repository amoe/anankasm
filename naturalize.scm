(require scheme/string)
(require scheme/pretty)
(require srfi/1)   ; list library
(require srfi/26)  ; cut & cute
(require scheme/system)

;(require (planet neil/levenshtein:1:1/levenshtein))

(require (prefix-in taglib: "taglib.scm"))

; orange JUICE
; orange JUICE
; orange JUICE
; for life!

(define *default-editor*   "/usr/bin/nano")
(define *default-eyed3*    "/usr/bin/eyeD3")
(define *default-mp3gain*  "/usr/bin/mp3gain")

(define *va-mode* #f)

(define *tag-map*
  (list
   (cons 'artist (cons taglib:tag-artist taglib:tag-set-artist))
   (cons 'album (cons taglib:tag-album taglib:tag-set-album))
   (cons 'tracknumber (cons taglib:tag-track taglib:tag-set-track))
   (cons 'title (cons taglib:tag-title taglib:tag-set-title))
   (cons 'date (cons taglib:tag-year taglib:tag-set-year))
   (cons 'genre (cons taglib:tag-genre taglib:tag-set-genre))))

(define *global-tag-list*
  (if *va-mode*
      '(album date genre)
      '(artist album date genre)))

(define *local-tag-list*
  (if *va-mode*
      '(tracknumber artist title)
      '(tracknumber title)))

; ENTRY POINT
(define (main . args)
  (preserve-mtimes
   (lambda ()
     (let ((tmpl (pass-to-editor (apply files->template args))))
       (strip-tags args)
       (apply-tags tmpl args)
       (replaygain args)
       (move-files args)))
   args))

(define (preserve-mtimes proc files)
  (let ((times (save-mtimes files)))
    (proc)
    (load-mtimes files times)))

(define (files->template . args)
  (define tags
    (map
     (lambda (tag)
       (cons tag
             (apply select-from-tags
                    (cons (lookup-getter tag) args))))
     *global-tag-list*))
     
  (define tracks
    (map
     (lambda (file)
       (map
        (lambda (tag)
          (tag-proc/cleanup (lookup-getter tag) file))
        *local-tag-list*))
     args))

  (cons tags tracks))

(define (pass-to-editor datum)
  (let ((path (make-temporary-file "naturalize-~a.scm")))
    (let ((out (open-output-file path #:exists 'truncate)))
      (pretty-print datum out)
      (close-output-port out)

      (run-editor path)

      (let ((result (read (open-input-file path))))
        (delete-file path)
        result))))

(define (strip-tags args)
  (apply system*
         (cons *default-eyed3*
               (cons "--remove-all"
                     (cons "--no-color" args)))))

(define (apply-tags tmpl files)
  (say "applying template: ~a" tmpl)

  (for-each
   (lambda (t) (apply-one-tag t files))
   (template:global-tags tmpl))

  (apply-local-tags (template:local-tags tmpl) files))

(define (replaygain files)
  ; igoldgain algorithm
  ; first, copy all files
  (let ((tmp (map
               (cute make-temporary-file "naturalize-~a.mp3" <>)
               files))
        (options (list "-s" "r" "-c" "-a")))
    (apply system*
           (cons *default-mp3gain*
                 (append options (map path->string tmp))))
    (for-each
      (lambda (orig copy)
        (say "applying RG tags to ~a from ~a" orig copy)
        
        (apply-text-tags (gain-info-from-tags copy)
                         orig))
      
      files (map path->string tmp))))

(define (move-files files)
  #t)

(define (gain-info-from-tags file)
  (let ((l (apply process* (list *default-mp3gain* "-s" "c" file))))
    (let ((output (slurp-lines (first l))))
      (list
       (cons 'replaygain_track_gain (after-colon (second output)))
       (cons 'replaygain_track_peak (after-colon (fourth output)))
       (cons 'replaygain_album_gain (after-colon (seventh output)))
       (cons 'replaygain_album_peak (after-colon (ninth output)))))))

(define (apply-text-tags tags file)
  (for-each
   (lambda (tag)
     (let ((str (format "--set-user-text-frame=~a:~a"
                        (car tag) (cdr tag))))
       (apply system* (list *default-eyed3* str file))))
   tags))

(define (after-colon str)
  (cadr (regexp-match #px".*:\\s*([-\\.\\d]+)" str)))

(define (slurp-lines port)
  (let ((l (read-line port)))
    (if (eof-object? l)
        '()
        (cons l (slurp-lines port)))))


(define (select-from-tags get-tag . args)
  (let ((tags (map (lambda (f) (tag-proc/cleanup get-tag f)) args)))
    (let ((hist (sort (histogram tags) frequency>?)))
      (if (just-one? hist)
          (caar hist)
          (ask-menu hist)))))


(define (lookup-getter tag)
  (cadr (assq tag *tag-map*)))

(define (lookup-setter tag)
  (cddr (assq tag *tag-map*)))


(define (save-mtimes files)
  (map file-or-directory-modify-seconds files))

(define (load-mtimes files times)
  (for-each file-or-directory-modify-seconds files times))

(define (run-editor file)
  (say "invoking editor: ~a" file)
  (system* (get-editor) (path->string file)))

(define (get-editor)
  (or (getenv "EDITOR") "/usr/bin/nano"))



(define (ask-menu hist)
  (print-histogram hist)
  (let ((answer (ask "Which tag is correct?" 1)))
    (car (list-ref hist (- answer 1)))))

(define (just-one? lst) (= (length lst) 1))

(define (ask question default)
  (display
    (format "~a [~a] " question default))
  (flush-output)
  (read))

(define (print-histogram hist)
  (for-each
    (lambda (n x f)
      (display
        (format "~a. ~a (~a)~n"
                n x f)))
    (iota (+ (length hist) 1) 1)
    (map car hist) (map cdr hist)))

; This is roughly a histogram, anyway - call it with a sorted list.
; It returns an alist of (item . frequency)
(define (histogram lst)
  (define (iter n item lst)
    (cond
     ((null? lst)  (cons (cons item n) '()))
     ((equal? (car lst) item)
      (iter (+ n 1) item (cdr lst)))
     (else (cons (cons item n)
                 (iter 1 (car lst) (cdr lst))))))

  (iter 0 (car lst) lst))

(define (frequency>? x y) (> (cdr x) (cdr y)))


; r5rs say
(define (say msg)
  (display msg)
  (newline))

; super-say
(define (say . args)
  (display (apply format args))
  (newline))

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
        *local-tag-list* local-tag))
    lt files))

(define (apply-one-tag tag files)
  (say "applying tag: ~a" tag)
  (for-each
   (lambda (file)
    (tag-proc/cleanup
     (let ((set-tag (lookup-setter (car tag))))
       (lambda (t) (set-tag t (cdr tag))))
     file))
   files))

(apply main (vector->list (current-command-line-arguments)))
