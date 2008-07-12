(require scheme/string)
(require scheme/pretty)
(require xml)
(require net/url)
(require file/gunzip)
(require srfi/1)   ; list library
(require srfi/26)  ; cut & cute
(require scheme/system)

(require (planet neil/levenshtein:1:1/levenshtein))

(require (prefix-in taglib: "taglib.scm"))

; orange JUICE
; orange JUICE
; orange JUICE
; for life!

(define (test:main)
  (main
   "/home/amoe/music/underworld/2007-oblivion_with_bells/01-crocodile.ogg"
   "/home/amoe/music/underworld/2007-oblivion_with_bells/02-beautiful_burnout.ogg"
   "/home/amoe/music/sigur_ros/2007-heim/06-von.ogg"))

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


(define (files->template . args)
  ;(define tags (map
   ;(lambda (x)
     
     ;(cons (car x) (apply select-from-tags (cons (cadr x) args))))
   
   ;*tag-map*))

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

(define (lookup-getter tag)
  (cadr (assq tag *tag-map*)))

(define (lookup-setter tag)
  (cddr (assq tag *tag-map*)))

(define (pass-to-editor datum)
  (let ((path (make-temporary-file "naturalize-~a.scm")))
    (let ((out (open-output-file path #:exists 'truncate)))
      (pretty-print datum out)
      (close-output-port out)

      (run-editor path)

      (let ((result (read (open-input-file path))))
        (delete-file path)
        result))))

(define (main . args)
  (preserve-mtimes
   (lambda ()
     (let ((tmpl (pass-to-editor (apply files->template args))))
       (apply-templates tmpl args)))
   args))


(define (preserve-mtimes proc files)
  (let ((times (save-mtimes files)))
    (proc)
    (load-mtimes files times)))

(define (save-mtimes files)
  (map file-or-directory-modify-seconds files))

(define (load-mtimes files times)
  (for-each file-or-directory-modify-seconds files times))


(define (run-editor file)
  (say "invoking editor: ~a" file)
  (system* (get-editor) (path->string file)))

(define (get-editor)
  (or (getenv "EDITOR") "/usr/bin/nano"))

(define (select-from-tags get-tag . args)
  (let ((tags (map (lambda (f) (tag-proc/cleanup get-tag f)) args)))
    (let ((hist (sort (histogram tags) frequency>?)))
      (if (just-one? hist)
          (caar hist)
          (ask-menu hist)))))

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

(define (tag-proc/cleanup proc file)
  (let ((f (taglib:file-new file)))
    (let ((datum (proc (taglib:file-tag f))))
      (taglib:file-save f)
      (taglib:file-free f)
      datum)))

(define template:global-tags car)
(define template:local-tags  cdr)

(define (apply-templates tmpl files)
  (say "applying template: ~a" tmpl)

  (for-each
   (lambda (t) (apply-one-tag t files))
   (template:global-tags tmpl))

  (apply-local-tags (template:local-tags tmpl) files))

(define local-tag:tracknumber car)
(define local-tag:title       cdr)

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

; (define (apply-local-tags lt files)
;   (for-each
;    (lambda (tag file)
;      (tag-proc/cleanup
;        (lambda (t)
;          (taglib:tag-set-track t (local-tag:tracknumber tag))
;          (taglib:tag-set-title t (local-tag:title tag)))
;        file))
;    lt files))
     

(define (apply-one-tag tag files)
  (say "applying tag: ~a" tag)
  (for-each
   (lambda (file)
    (tag-proc/cleanup
     (let ((set-tag (lookup-setter (car tag))))
       (lambda (t) (set-tag t (cdr tag))))
     file))
   files))
   







;;;;;;;;;;;;;;; OLD CODE
;;;;;;;;;;;;;;;;; BEWARE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define *debug-mode* #t)

(define (debug msg)
  (when *debug-mode*
    (display msg)
    (newline)))

(define (main-2 . args)
  (let ((release (apply process-files args)))
    (handle-search-results
     (fetch (construct-search-query release "releases")))))

(define (process-files . args)
  (cond
   ((null? args)  (error "no files given on command line"))
   (else
    (confirm-release
     (sort
      (histogram (sort (map get-album args) string<?))
      frequency>?)))))

(define (just-one? hist) (= (length hist) 1))

(define (confirm-release hist)
  (cond
   ((just-one? hist) (caar hist))
   (else
    (say "Tag disparity detected.")
    
    (let ((v (list->vector hist)))
      (for-each say (format-histogram hist))
      (let ((i (ask "Which release?" 1)))
        (car (vector-ref v (- i 1))))))))

(define (frequency>? x y) (> (cdr x) (cdr y)))

(define (format-histogram hist)
  (map (lambda (c s n) (format "~a. ~a (~a)" c s n))
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


; Get release & take care of cleanup
(define (get-album file)
  (let ((f (taglib:file-new file)))
    (let ((a (taglib:tag-album (taglib:file-tag f))))
      (taglib:file-free f)
      a)))

; Extract tags using taglib.
; Query Discogs with artist which is generally correct.
; Ask user to pick an item if several results, provide sane default.
; Do same search for release within artist page.
; Query on release.  Extract tags.

; Apply tags - may need to shell out to id3v2.
; No, we can do it with taglib.
; But we will eventually need to shell out to eyeD3 to apply replaygain tags.

; Maybe we just improve igoldgain and shell out to that?
; It might be easier, since we're definitely going to need to do it at the end
; anyway.

(define *editor* (getenv "EDITOR"))

(define mp3gain "mp3gain")

(define *api-key* "fc618f80ac")

(define req
  (string-append "http://www.discogs.com/release/1?f=xml&api_key="
                 *api-key*))

; Algorithm
; (lookup-discogs)
; (hand-to-editor)
; (apply-tags)
; (run-replaygain)
; (move-files)

(define (construct-search-query string type)
  (string-append (uri->api-uri "http://www.discogs.com/search")
                 ";q=" string ";type=" type))

(define (uri->api-uri u)
  (string-append u "?f=xml;api_key=" *api-key*))

;(when (not e)
  ;(error "please define EDITOR in the environment"))

(define (_main . args)
  (for-each
   (lambda (x)
     (handle-search-results
      (fetch (construct-search-query x "releases")))) args))


(define first-child caddr)
(define children cddr)

; FIXME: use levenshtein distance to choose default
(define (handle-search-results xexpr)
  (define r (children (first-child xexpr)))
  (define rv (list->vector r))
  
  (for-each say
            (map format-release
                 (iota (vector-length rv) 1)
                 (map (compose first-child first-child) r)))

  (let ((i (ask "Which release is it?" 1)))
    (say (format "you said: ~a" i))
    (say (format "~s" (fetch-release
     (first-child
          (assq 'uri
                (children (vector-ref rv (- i 1))))))))))

(define (fetch-release uri)
  (let ((u (uri->api-uri uri)))
    (say (format "fetching ~a" u))
    (fetch u)))

; conver release xexpr to template
(define (release->template x-release)
  (list
   (cons 'artist (release:artist x-release))))

(define (release:artist x-release)
  (string-join
   (map (compose first-child first-child)
        (children
         (assq 'artists
               (children (first-child x-release)))))
   " / "))

(define (format-release number name)
  (format "~a. ~a" (number->string number) name))

; r5rs say
(define (say msg)
  (display msg)
  (newline))

; super-say
(define (say . args)
  (display (apply format args))
  (newline))

(define (handle input-port)
  (xml->xexpr
   (document-element
   (read-xml
    (open-input-bytes
     (let ((p (open-output-bytes)))
       (gunzip-through-ports input-port p)
       (get-output-bytes p)))))))


(define (fetch str)
  (display (format "Fetching URL: ~a~n" str))
   (call/input-url (string->url str)
                   get-pure-port
                   handle
                   '("Accept-Encoding: gzip")))


(define (track-gain file)
  (let ((tmp (make-temporary-file "naturalize-~a" file)))
    ; rip apart RG info
    (system (format "mp3gain -s r -o ~a" tmp))
    (delete-file tmp)))

(apply main (vector->list (current-command-line-arguments)))

;(file-stream-buffer-mode (current-input-port) 'none)
;(ask)

