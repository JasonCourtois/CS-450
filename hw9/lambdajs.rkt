#!/usr/bin/env mzscheme
#lang racket

(require "interp.rkt")

(define fname (car (vector->list (current-command-line-arguments))))

; Suppresses printing the result (which is invariably undefined)
(display ((interp empty-env) (file->list fname)))
