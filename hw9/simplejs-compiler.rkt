#!/usr/bin/env mzscheme
#lang racket
(require "hw9-util.rkt")
(require "hw9-parse.rkt")
(require "hw9.rkt")

(define (s:compile x)
  (j:quote (translate (s:parse x))))

(define fname (car (vector->list (current-command-line-arguments))))

(printf "~s\n" (s:compile (first (file->list fname))))
