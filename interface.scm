(module interface scheme
  (require scheme/system)
  (require srfi/1)

  (require "options.scm")

  (provide run-command)

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
  
  (define (say msg)
    (display msg)
    (newline))


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
          
  (define (choose list default)
    (print-menu list)
    (printf "Pick one, default ~a: " default)
    (flush-input)  ; fix repl
    (pick-response list default))

  ; eli barzilay's code - deep dark magic
  ; http://www.cs.brown.edu/pipermail/plt-scheme/2006-January/011093.html
  (define flush-input
    (let ((buf (make-bytes 10)))
      (lambda ()
        (let loop ()
          (let ((n (read-bytes-avail!* buf)))
            (unless (or (eof-object? n) (zero? (read-bytes-avail!* buf)))
              (loop)))))))

  
  (define (print-menu list)
    (for-each
      (lambda (number item)
        (printf "~a. ~a~n" number item))
      (iota (length list) 1)
      list)))
