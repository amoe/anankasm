(require xml)
(require net/url)
(require file/gunzip)

;(require (planet neil/levenshtein:1:1/levenshtein))

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


(define (format-histogram hist)
  (map (lambda (c s n) (format "~a. ~a (~a)" c s n))
       (iota (+ (length hist) 1) 1)
       (map car hist) (map cdr hist)))


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
