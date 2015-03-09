#lang racket


(provide histogram frequency>?)


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

