#lang scheme

(require scheme/system)
(require scheme/pretty)

(require srfi/1)   ; why?

(require "options.scm")

(provide
 run-command
 pass-to-editor
 choose
 say
 debug
 writable?
 xformat
 slurp-lines
 *default-eyed3*
 *default-mp3gain*)

(define *default-editor* "/usr/bin/nano")
(define *default-eyed3*    "/usr/bin/eyeD3")
(define *default-mp3gain*  "/usr/bin/mp3gain")

(define (pass-to-editor datum)
  (let ((path (make-temporary-file "naturalize-~a.scm")))
    (let ((out (open-output-file path #:exists 'truncate)))
      (pretty-print datum out)
      (close-output-port out)
      
      (run-editor path)
      
      (let ((result (read (open-input-file path))))
        (delete-file path)
        result))))


; Run editor, which can be an arbitrary shell command
; 'shell-quote' is used to escape arguments
(define (run-editor file)
  (system
   (string-append (get-editor)
                  (string #\space)
                  (shell-quote (path->string file)))))

(define (get-editor)
  (or (getenv "EDITOR") *default-editor*))

(define (run-command . args)
  (apply
   (if (option 'verbose) system* system/silent*)
   args))

; NB:
; mp3gain(1) has a freakin' weird behaviour where unless you read the data from
; stderr, a write(2) call will eventually block and freeze the process at S+
; status on ps(1).  Hence the CONSUME call here; if you remove this, mp3gain
; processes will mysteriously hang on large (> 4 files) input sets.
(define (system/silent* . args)
  (let ((l (apply process* args)))
    (consume (fourth l))

    ((fifth l) 'wait)
    
    (close-input-port (first l))
    (close-output-port (second l))
    (close-input-port (fourth l))))
      
(define (consume port)
  (let ((b (read-byte port)))
    (when (not (eof-object? b))
      (consume port))))
  
; super-say
  (define (say . args)
    (display (apply format args))
    (newline))

  (define (debug msg)
    (when (option 'debug) (say msg)))

  (define (xformat str alist)
  (regexp-replace* #rx"%(.)" str 
                   (lambda (all one)
                     (let ((r (assoc one alist)))
                       (if r (cdr r) "")))))


(define (choose list default)
    (print-menu list)
    (printf "Pick one, default ~a: " default)
    (flush-output)
    (flush-input)  ; fix repl
    (pick-response list default))

(define (print-menu list)
    (for-each
      (lambda (number item)
        (printf "~a. ~a~n" number item))
      (iota (length list) 1)
      list))

(define (pick-response list default)
  (let ((resp (read-line)))
    (if (string=? resp "")
        (list-ref list (sub1 default))
        (let ((datum (read (open-input-string resp))))
          (cond
           ((string? datum)  datum)
           ((exact-positive-integer? datum)
            (list-ref list (sub1 datum)))
           (else
            (error "can't understand response")))))))
  
  ; eli barzilay's code - deep dark magic
  ; http://www.cs.brown.edu/pipermail/plt-scheme/2006-January/011093.html
  (define flush-input
    (let ((buf (make-bytes 10)))
      (lambda ()
        (let loop ()
          (let ((n (read-bytes-avail!* buf)))
            (unless (or (eof-object? n) (zero? (read-bytes-avail!* buf)))
              (loop)))))))

; Shell metacharacters, from POSIX
(define metacharacters
  "|&;<>()$`\"' \t\n*?[#~=%")

(define meta-re
  (regexp
    (string-join
     (map (compose regexp-quote string) (string->list metacharacters))
     "|")))

(define (shell-quote s)
  (regexp-replace*
    meta-re s (lambda args (string-append "\\" (first args)))))

; input = list of lists:
; first = short-option, second = long-option, third = description
; since short opts are always one char, just calculate the longest.
; then spaces after opts is (length-of-longest - length-of-this-one) + 4
; warn if any options spill over 80 chars
(define (format-usage opts)
  ; We can't use max, since we also need to kow the indx of what we seek
  (apply max (map (compose string-length second) opts)))

(define (writable? path)
  (->boolean (memq 'write (file-or-directory-permissions path))))

(define (->boolean x)
  (if x #t #f))

(define (slurp-lines port)
  (let ((l (read-line port)))
    (if (eof-object? l)
        '()
        (cons l (slurp-lines port)))))
