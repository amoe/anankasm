#! /usr/bin/env mzscheme

#lang racket
; This file is used as the test script during development

(require "naturalize.scm")

(apply main (vector->list (current-command-line-arguments)))
