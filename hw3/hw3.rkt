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
    [(<= (length l) 1) l] ;; If list has 0 or 1 items, return the list itself.
    [else 
        (foldr 
          (lambda (nextItem total) 
            (cond
              [(= 0 (length total))
                (list nextItem)]  ;; If the current total is empty, create a list with just the last item in the list.
              [else
                (cons nextItem (cons v total))])) ;; (cons v total) adds the separator to the front of the running list. Then put the next item in front of that.
          (list) l)]))

;; Exercise 8
(define (parse-ast node)
  (define (make-define-func node)
    (define function (first (second node)))
    (define parsed-function (parse-ast function))
    
    (define arguments (rest (second node)))
    (define parsed-arguments (map parse-ast arguments))
    
    (define body (rest (rest node)))
    (define parsed-body (map parse-ast body))

    (r:define parsed-function (r:lambda parsed-arguments parsed-body))
    )

  (define (make-define-basic node)
    ;; Get the name of the variable and parse it.
    (define name (first node))
    (define parsed-name (parse-ast name))
    ;; Get the value of the variable and parse it.
    (define value (second node))
    (define parsed-value (parse-ast value))
    ;; Create define AST node.
    (r:define parsed-name parsed-value))

  (define (make-lambda node)
    ;; Get list of arguments, and then use map to parse each one.
    (define arguments (second node))
    (define parsed-arguments (map parse-ast arguments))
    ;; Get list of body statements, and then use map to parse each one.
    (define body (rest (rest node)))
    (define parsed-body (map parse-ast body))
    ;; Create lambda AST node.
    (r:lambda parsed-arguments parsed-body))

  (define (make-apply node)
    ;; Get name of function and parse it.
    (define function (first node))
    (define parsed-function (parse-ast function)))
    ;; Get list of arguments, and then use map to parse each one.
    (define arguments (rest node))
    (define parsed-arguments (map parse-ast arguments))
    ;; Create apply AST node.
    (r:apply parsed-function parsed-arguments)

  (define (make-number node)
    ;; Create a number using the node input which should be a number like 'x
    (r:number node))

  (define (make-variable node)
    ;; Create a variable using the node input which should be a symbol like 'x
    (r:variable node))

  ;; Don't change the below functions
  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
