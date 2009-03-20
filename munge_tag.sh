#!/usr/bin/env mzscheme

#lang scheme

(require (file "../lib/naturalize/munge-tag.scm"))

(let loop ((l (read-line)))
  (cond
    ((eof-object? l)  (void))
    (else
      (display (munge-tag l))
      (newline)
      (loop (read-line)))))
