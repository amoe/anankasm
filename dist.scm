#! /usr/bin/env mzscheme

; This file gets installed in 'bin' on FHS-compliant Unix

(require "../lib/naturalize.scm")

(apply main (vector->list (current-command-line-arguments)))
