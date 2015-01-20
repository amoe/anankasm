#lang racket/base

(require rackunit "../component1.rkt")

(check-equal? (meaning-of-life) 42)
