#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^


;; Exercise 1
(define (min-from n l) 'todo)

;; Exercise 2
(define (count l) 'todo)

;; Exercise 3
(define (sum l) 'todo)

;; Exercise 4
(define (occurrences n l) 'todo)

;; Exercise 5
(define (prefix s l) 'todo)

;; Exercise 6
(define (interleave l1 l2)
  (match l1
    [(list)
      'todo]
    [(list h1 l1 ...)
      'todo])
)

;; Exercise 7
(define (intersperse l v) 'todo)

;; Exercise 8
(define (parse-ast node)
  (define (make-define-func node) 'todo)
  (define (make-define-basic node) 'todo)
  (define (make-lambda node) 'todo)
  (define (make-apply node) 'todo)
  (define (make-number node) 'todo)
  (define (make-variable node) 'todo)

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
