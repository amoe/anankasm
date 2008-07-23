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
    (let ((l (apply process* args)))
      ((fifth l) 'wait)
      (close-input-port (first l))
      (close-output-port (second l))
      (close-input-port (fourth l))
      ((fifth l) 'exit-code)))
  
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
              (loop))))))))
