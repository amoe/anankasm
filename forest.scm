#! /usr/bin/env mzscheme

#lang scheme

(require srfi/1)
(require srfi/26)

(require "interface.scm")

; BUGS:
;   make-parents creates dirs that do not have the mtime of their duplicates
;   no relative pathnames, must be absolute

; gened with df -B1
(define *sansa-size* 8005644288)

(define (gigabytes x)
  (* x (expt 2 30)))

(define *limit* *sansa-size*)

(define (main . args)
  (let ((old (first args))
        (new (second args)))
    (say "building forest from ~a to ~a" old new)
    (say "warning: any existing forest in ~a will be nuked" new)
    (delete-directory/files new)
    (for-each
      (lambda (dir)
        (let ((l (convert-path old new dir)))
          (make-parents l)
          (make-file-or-directory-link dir l)))
      (accumulate-to-limit
       (sort-by-mtime (find-leaves old))
       *limit*))))

(define (convert-path old new path)
  (simplify-path (build-path new (path-remove-prefix old path))))

; Remove leading PREFIX from PATH
; PREFIX and PATH are both paths
(define (path-remove-prefix prefix path)
  (apply (cut build-path 'same <...>)
         (let loop ((p1 (explode-path prefix))
             (p2 (explode-path path)))
           (cond
             ((null? p1)  p2)
             ((null? p2)  path)   ; this is a warn condition
             ((equal? (car p1) (car p2))
               (loop (cdr p1) (cdr p2)))
             (else
               (cons (car p2) (loop (cdr p1) (cdr p2))))))))
               
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

(define (identity x) x)

; WARNING: DARK DARK MAGIC
(define (make-parents path)
  (for-each make-directory/uncaring
    (map (cute apply build-path <>)
         (map reverse
              (unfold-right
                null?
                identity
                cdr
                (cdr (reverse (explode-path path))))))))

; warning: race condition
(define (make-directory/uncaring path)
  (when (not (directory-exists? path))
    (make-directory path)))

(apply main (vector->list (current-command-line-arguments)))
