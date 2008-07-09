(require scheme/string)
(require scheme/pretty)
(require xml)
(require net/url)
(require file/gunzip)
(require srfi/1)

(require (planet neil/levenshtein:1:1/levenshtein))

; XXX plt v3 remnants
(require (lib "process.ss"))
(require (lib "file.ss"))

(require (prefix-in taglib: "taglib.scm"))


(define (test:main)
  (main
   "/home/amoe/music/underworld/2007-oblivion_with_bells/01-crocodile.ogg"
   "/home/amoe/music/underworld/2007-oblivion_with_bells/02-beautiful_burnout.ogg"
   "/home/amoe/music/sigur_ros/2007-heim/06-von.ogg"))

(define *tag-map*
  (list
   (cons 'artist (cons taglib:tag-artist taglib:tag-set-artist))
   (cons 'album (cons taglib:tag-album taglib:tag-set-album))
   (cons 'date (cons taglib:tag-year taglib:tag-set-year))
   (cons 'genre (cons taglib:tag-genre taglib:tag-set-genre))))

(define (files->template . args)
  (define tags (map
   (lambda (x)
     (cons (car x) (apply select-from-tags (cons (cadr x) args))))
   *tag-map*))
     
  (define tracks
    (map
     (lambda (x) (tag-proc/cleanup taglib:tag-title x))
     args))

  (pretty-print (cons tags tracks)))

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

(define (tag-proc/cleanup get-tag file)
  (let ((f (taglib:file-new file)))
    (let ((datum (get-tag (taglib:file-tag f))))
      (taglib:file-free f)
      datum)))




















;   NEXT:
; Implement mode that skips lookup, fills in template based on tags,
; and dumps you into editor.

; Album Mode
; Unrelated Mode
; -- skips discogs search and dumps you into an editor w/current tags

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

(define main files->template)
(apply main (vector->list (current-command-line-arguments)))

;(file-stream-buffer-mode (current-input-port) 'none)
;(ask)

