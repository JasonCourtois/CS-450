#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

  We ask that solutions be distributed only locally -- on paper, on a
  password-protected webpage, etc.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

                    * * * ATTENTION! * * *

  Every solution submitted to our grading server is automatically compared
  against a solution database for plagiarism, which includes every solution
  from every student in past semesters.

  WE FOLLOW A ZERO-TOLERANCE POLICY: any student breaking the Code of Student
  Conduct will get an F in this course and will be reported according to
  Section II Academic Dishonesty Procedures.

|#

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

;; Original Equation was ((13−1)+10)−(6+(5+11))
(define ex1 
  (- 
    (+ 
      (- 13 1) 
      10) 
    (+ 
      6 
      (+ 5 11))))

(define ex2 
  (list 
    ;; Initial Equation
    (- 
      (+ 
        (- 13 1) 
        10) 
      (+ 
        6 
        (+ 5 11)))
    ;; Compute 13 - 1
    (- 
      (+ 12 10) 
      (+ 
        6 
        (+ 5 11)))
    ;; Compute 12 + 10
    (- 
      22 
      (+ 
        6 
        (+ 5 11)))
    ;; Compute 5 + 11
    (- 
      22 
      (+ 6 16))
    ;; Compute 6 + 16
    (- 22 22)
    ;; Compute 22 - 22
    0))

;; Original Function
;; def ex3(x, y):
;;     return (x * 4) - x == (y - 6) + (x - 6)
(define
  (ex3 x y) 
    ;; Function takes x and y input, and computes (x * 4) - x == (y - 6) + (x - 6)
    (= 
      (- 
        (* x 4) 
        x) 
      (+ 
        (- y 6) 
        (- x 6))))

;; EXERCISE 4

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))
;; Constructs a tree no children
(define (tree-leaf value) (list null value null))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) 
  (tree (tree-left self) value (tree-right self)))

(define (tree-set-left self left) 
  (tree left (tree-value self) (tree-right self)))

(define (tree-set-right self right) 
  (tree (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value)
  ;; The equivalent Python code is above each line for reference on how syntax was chosen.
  (cond 
    ;; if node is None:
    ;;  return Tree(None, value, None) - This is equivalent to our tree-leaf constructor.
    [(null? self) 
      (tree-leaf value)]
    
    ;; if value == node.value:
    ;;  return node.set_value(value)
    [(= value (tree-value self)) 
      (tree-set-value self value)]
    
    ;; if value < node.value:
    ;;  return node.set_left(insert(node.left, value))
    [(< value (tree-value self)) 
      (tree-set-left self (bst-insert (tree-left self) value))]
    
    ;; return node.set_right(insert(node.right, value)) 
    [else 
      (tree-set-right self (bst-insert (tree-right self) value))]))

;; EXERCISE 5

;; lambda
(define (lambda? node) 
  (and
      ;; Check if the input is a list.
      (list? node)
      ;; Lambda definition will have at least 3 items.
      (>= (length node) 3)
      ;; The first item should always be the symbol 'lambda.
      (equal? 'lambda (first node))
      ;; The parameters should be a list of symbols.
      (list? (lambda-params node))
      (andmap symbol? (lambda-params node))
      ;; There should be at least one item in the body
      (>= (length (lambda-body node)) 1)))

(define (lambda-params node) (second node)) ;; List of params in lambda def is the second item
(define (lambda-body node) (rest (rest node))) ;; The remaining body of the lambda is after the first two items.

;; apply
(define (apply? l) 
  (and 
    ;; Every function application should be a list that is at least 1 item long.
    (list? l)
    (>= (length l) 1)
    ;; The first item in the function should follow lambda syntax or be a symbol.
    (or 
      (lambda? (apply-func l))
      (symbol? (apply-func l)))))

(define (apply-func node) (first node)) ;; Returns symbol of function being called
(define (apply-args node) (rest node))  ;; Returns list of arguments passed into function.

;; define
(define (define? node) 
  ;; All defines will either be a basic or function definition.
  (or 
    (define-basic? node)
    (define-func? node)))

(define (define-basic? node) 
  (and
    ;; Check if the input is a list
    (list? node)
    ;; A basic definition will always have 3 items.
    (equal? (length node) 3)
    ;; The first item should always be the symbol 'define
    (equal? 'define (first node))
    ;; The second item should be any symbol for the name of the variable.
    (symbol? (second node))))

(define (define-func? node) 
  (and
      ;; Check if the input is a list
      (list? node)
      ;; A function definition will always have at least 3 items.
      (>= (length node) 3)
      ;; The first item should always be the symbol 'define
      (equal? 'define (first node))
      ;; The second item should be a list with at least one item, which should all be symbols.
      (list? (second node))
      (>= (length (second node)) 1)
      (andmap symbol? (second node))))

#|

EXERCISE 6

1. Input the following prompt in a Generative-AI tool such as Claude, ChatGPT,
   or Deepseek.
2. Paste below ANSWER the 4 test cases and review each test in terms of
   correcntesss, that is, should the test pass or fail?
3. Take a screenshot and upload your interaction to Gradescope

PROMPT:

Suppose that I am implementing function `define-basic?`. Below are some test
cases. Help me generate 4 new test cases: 2 positive tests and 2 negative tests.
```
; 4.h)
(define-test-suite test-4.h
  (test-case "Exercise 4.h"
    (check-true (define-basic? (quote (define x 3))))
    (check-false (define-basic? '(define)))
    (check-false (define-basic? '(define 1)))
    (check-false (define-basic? '(define 1 2)))
    (check-false (define-basic? '(define () 2)))
    (check-false (define-basic? '(define (1) 2)))
    (check-false (define-basic? '(define (x) 2)))
    (check-false (define-basic? '(define (x 1) 2)))
    (check-false (define-basic? '(1 define 2)))
    (check-false (define-basic? '(1 2 define)))
    (check-false (define-basic? '(define 2 define)))
    (check-true (define-basic? '(define define 2)))
    (check-true (define-basic? '(define define define)))
  )
)
```

ANSWER:

Link to my ChatGPT conversation: https://chatgpt.com/share/68c84a26-5020-8013-bb5b-0e757ea360bf

Here is what ChatGPT gave me for the positive and negative test cases:

------ First Positive Case ------
(check-true (define-basic? '(define y 42)))

Correct? - Yes
This test case is valid, the code ```define y 42``` is a correct use of a basic definition.

------ Second Positive Case ------
(check-true (define-basic? '(define mylist '(1 2 3))))

Correct? - Yes
This test case is also valid, the code ```define mylist '(1 2 3)``` is a correct way to define a quoted list in racket.

------ First Negative Case ------
(check-false (define-basic? '(define z)))

Correct? - Yes
In this test case, there is no value given for the identifier z 

------ Second Negative Case ------
(check-false (define-basic? '(define "name" 5)))

Correct? - Yes
Here, the variable name is given as a string literal and not a symbol, which works as a false test case.

Overall I was surprised that ChatGPT was able to understand what programming language was being used without the prompt mentioning racket.
I was also surprised that it correctly generated 4 working test cases, I tested them with my working code and they all passed properly.

|#
