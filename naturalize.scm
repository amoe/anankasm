(require xml)
(require scheme/string)
(require (planet neil/levenshtein:1:1/levenshtein))
(require net/url)
(require file/gunzip)
(require srfi/1)
(require (prefix-in taglib: "taglib-2.scm"))
(require (lib "process.ss"))
(require (lib "file.ss"))
;(require (lib "url.ss" "net"))

; Album Mode
; Unrelated Mode
; -- skips discogs search and dumps you into an editor

(define *debug-mode* #t)

(define (debug msg)
  (when *debug-mode*
    (display msg)
    (newline)))

(define (process-files . args)
  (confirm-release 
  (histogram (sort (map get-album args) string<?))))

(define (format-histogram hist)
  (map (lambda (c s n) (format "~a. ~a (~a)" c s n))
       (iota (+ (length hist) 1) 1)
       (map car hist) (map cdr hist)))

(define (confirm-release hist)
  (say
  (if (> (length hist) 1)
      (begin
        (for-each say (format-histogram hist))
        (say "Tag disparity detected.")
        (ask "Which release is correct?" (caar hist)))
      (caar hist))))

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

(define (main . args)
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


; unbreak repl
(define (flush-and-read-line)
  (let ((buf (make-bytes 10)))
    (lambda ()
      (let loop ()
        (let ((n (read-bytes-avail!* buf)))
          (unless (or (eof-object? n) (zero? (read-bytes-avail!* buf)))
            (loop))))))

  (read-line))

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

(define (ask msg default)
  (display (format "~a [~a]  "
                   msg default))
  (flush-output)
  (read))

(define (say msg)
  (display msg)
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

(define main process-files)

(apply main (vector->list (current-command-line-arguments)))

;(file-stream-buffer-mode (current-input-port) 'none)
;(ask)

