#!/usr/bin/env mzscheme
#lang racket
(require "hw9-util.rkt")
(require "hw9-parse.rkt")
(require "hw9.rkt")
(require "interp.rkt")

(define (j:eval js [env empty-env])
  ((interp env) (j:quote js)))

(define (s:eval x [env empty-env])
  (j:eval (translate (s:parse x)) env))

(define fname (car (vector->list (current-command-line-arguments))))

; Suppresses printing the result (which is invariably undefined)
(display (s:eval (first (file->list fname))))
