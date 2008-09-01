(module interface scheme
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
    xformat)

  (define *default-editor* "/usr/bin/nano")

  (define (pass-to-editor datum)
    (let ((path (make-temporary-file "naturalize-~a.scm")))
      (let ((out (open-output-file path #:exists 'truncate)))
        (pretty-print datum out)
        (close-output-port out)
        
        (run-editor path)
        
        (let ((result (read (open-input-file path))))
          (delete-file path)
          result))))

  (define (run-editor file)
    (system* (get-editor) (path->string file)))

  (define (get-editor)
    (or (getenv "EDITOR") *default-editor*))

  (define (run-command . args)
    (apply
      (if (option 'verbose) system* system/silent*)
      args))

  (define (system/silent* . args)
    (debug "using new system/silent*")

    (let ((l (apply process* args)))
      (close-output-port (second l))
      (close-input-port (first l))

      ((fifth l) 'wait)

      (close-input-port (fourth l))))

      
     
      
  
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


; input = list of lists:
; first = short-option, second = long-option, third = description
; since short opts are always one char, just calculate the longest.
; then spaces after opts is (length-of-longest - length-of-this-one) + 4
; warn if any options spill over 80 chars
(define (format-usage opts)
  ; We can't use max, since we also need to kow the indx of what we seek
  (apply max (map (compose string-length second) opts))))
