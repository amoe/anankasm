#! /usr/bin/env mzscheme

#lang scheme

(require srfi/1)
(require srfi/26)

(require "interface.scm")

;

(define (main . args)
  (let ((old (first args))
        (new (second args)))
    (say "building forest from ~a to ~a" old new)
    (say "warning: existing forest in ~a will be nuked" old)))

; build alist: leaf -> mtime, sort on mtime, cdr down the list accumulating a
; new one, stop when predicate is true.  possibly the span and break procs?

; The mtime of a leaf-dir is the highest mtime in it
(define (leaf-dir:mtime path)
  (apply max
         (map file-or-directory-modify-seconds
              (directory-list/full path))))

; The size of a leaf-dir is the sum of the size of its component files
(define (leaf-dir:size  path)
  (fold + 0 (map file-size (directory-list/full path))))

; A dir is a leaf-dir if it has no subdirs
(define (leaf-dir? path)
  (and
    (directory-exists? path)    ; it is a dir
    (not (any directory-exists? (directory-list/full path)))))

(define (sort-by-mtime leaves)
  (map car
       (sort
         (alist-map leaf-dir:mtime leaves)
         (lambda (x y)
           (> (cdr x) (cdr y))))))

(define (gigabytes x)
  (* x (expt 2 30)))

(define (accumulate-to-limit paths limit)
  (let loop ((total 0) (paths paths))
    (if (null? paths)
        '()
        (let* ((path (car paths))
               (size (+ total (leaf-dir:size path))))
          (if (> size limit)
              '()
              (cons path (loop size (cdr paths))))))))

(define (find-leaves root)
  (find-files leaf-dir? root))

(define (directory-list/full path)
  (map (cut build-path path <>) (directory-list path)))

; restricted alist mapper
; it's now possible to write, say, (alist-map leaf-dir:mtime leaf-dirs)

; NOTE: *only* the leaf dirs are symlinks, parent dirs are real
(define (alist-map proc lst)
  (map (lambda (x) (cons x (proc x))) lst))

;(apply main (vector->list (current-command-line-arguments)))
