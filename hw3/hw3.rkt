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
(define (min-from n l) 
  (foldl min n l))

;; Exercise 2
(define (count l) 
  ;; Calls foldl with a lambda that accepts the next item and the running total.
  ;; The next item is unused as we only need the count, so 1 is added to the running total.
  (foldl (lambda (nextItem total) (+ total 1)) 0 l))

;; Exercise 3
(define (sum l) 
  ;; Calls foldl with a lambda that accepts the next item and the running total - like the last problem.
  ;; However here instead of adding 1, I add the value of the next item in the list.
  (foldl (lambda (nextItem total) (+ nextItem total)) 0 l))

;; Exercise 4
(define (occurrences n l) 
  (foldl 
    ;; foldl function.
    (lambda (nextItem total) 
      (cond 
        [(= n nextItem) (+ total 1)]  ;; Increment total if nextItem is equal to n.
        [else total]))  ;; Otherwise, return the existing total.
      ;; foldl arguments.
      0 l))

;; Exercise 5
(define (prefix s l)
  ;; Map applies this lambda to every item in list.
  (map 
    ;; Lambda adds the prefix s to the item that was passed in.
    (lambda (nextItem) (string-append s nextItem))
    l))

;; Exercise 6
(define (interleave l1 l2)
  (match l1
    [(list)
      l2] ;; If the first list is empty, then return the second list.
    [(list h1 l1 ...)
      (define result (interleave l2 l1))  ;; Call recursively with flipped list order. 
      (cons h1 result)]))  ;; Add h1 to list followed by result of the recursive call.

;; Exercise 7
;; Do not check for the last element, should be checking only for the first element.
(define (intersperse l v) 
  (cond 
    [(<= (length l) 1) l]
    [else 
      (cons
        (first l)
        (foldl (lambda (nextItem total) (cons (cons v nextItem) total)) (list) l))]))

(intersperse (list 1 2 3) 0 )

;; Exercise 8
(define (parse-ast node)
  (define (make-define-func node) 'todo)
  (define (make-define-basic node) 'todo)
  (define (make-lambda node) 'todo)
  (define (make-apply node) 'todo)
  (define (make-number node) 'todo)
  (define (make-variable node) 'todo)

  ;; Don't change the below functions
  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
