(module interface scheme
  (require scheme/system)
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
