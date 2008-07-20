#! /usr/bin/env mzscheme

(require "naturalize.scm")

(apply main (vector->list (current-command-line-arguments)))
