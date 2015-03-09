#! /usr/bin/env mzscheme

#lang racket

(current-library-collection-paths
 (list (string->path "/home/amoe")))

(require (lib "ripsys/munge-tag.scm"))
  "
;(require "../lib/naturalize/munge-tag.scm")

;(dynamic-require
  ;'(file "/home/amoe/ripsys/munge-tag.scm") 
  ;'munge-tag)

;(display (munge-tag "foo bar"))
;(newline)

(display (find-executable-path (find-system-path 'run-file)))
(newline)

(define (basename path)
  (define-values (base name must-be-dir?) (split-path path))
  base)

;(define (lib-path binpath)
  (define-values (base name must-be-dir?) (split-path binpath))
  (simplify-path
    (build-path
      (build-path base 'up)
      "lib"
      "naturalize")))
  
