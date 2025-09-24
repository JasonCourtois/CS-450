#lang racket

#|
            ######################################################
            ###  PLEASE DO NOT DISTRIBUTE TEST CASES PUBLICLY  ###
            ###         SHARING IN THE FORUM ENCOURAGED        ###
            ######################################################

  You are encouraged to share your test cases in the course forum, but please
  do not share your test cases publicly (eg, GitHub), as that stops future
  students from learning how to write test cases, which is a crucial part of
  this course.
|#

(require rackunit)
(require rackunit/text-ui)
(require "hw1.rkt")
; 4)
(define-test-suite test-4
  (test-case "Exercise 4."
    ; tree function tests
    (check-equal? (tree null 5 null) (list null 5 null))
    (check-equal? (tree (list null 1 null) 2 (list null 3 null)) (list (list null 1 null) 2 (list null 3 null)))
    
    ; tree-leaf function tests  
    (define bst7 (tree-leaf 7))
    (check-equal? bst7 (list null 7 null))
    
    ; tree accessor tests
    (check-equal? (tree-left bst7) null)
    (check-equal? (tree-value bst7) 7)
    (check-equal? (tree-right bst7) null)

    ; combined tree-lead+tree
    (define complex-tree (tree (tree-leaf 1) 2 (tree-leaf 3)))
    (check-equal? (tree-left complex-tree) (list null 1 null))
    (check-equal? (tree-value complex-tree) 2)
    (check-equal? (tree-right complex-tree) (list null 3 null))
    
    ; tree setter tests
    (check-equal? (tree-set-value bst7 99) (list null 99 null))
    (check-equal? (tree-set-left bst7 (tree-leaf 5)) (list (list null 5 null) 7 null))
    (check-equal? (tree-set-right bst7 (tree-leaf 9)) (list null 7 (list null 9 null)))
    (define original (tree (tree-leaf 1) 2 (tree-leaf 3)))
    (check-equal? (tree-set-value original 100) (tree (tree-leaf 1) 100 (tree-leaf 3)))
    
    ; bst-insert tests
    (define bst76 (bst-insert bst7 6))
    (check-equal? bst76 (tree (tree-leaf 6) 7 null))

    (define bst768 (bst-insert bst76 8))
    (check-equal? bst768 (tree (tree-leaf 6) 7 (tree-leaf 8)))

    (define bst7685 (bst-insert bst768 5))
    (check-equal? bst7685 (tree (tree (tree-leaf 5) 6 null) 7 (tree-leaf 8)))
    (check-equal? (bst-insert bst7685 7) bst7685)
    (check-equal? (bst-insert null 10) (tree-leaf 10))
    (check-equal? (bst-insert (tree-leaf 5) 3) (tree (tree-leaf 3) 5 null))
  )
)

; 5.a)
(define-test-suite test-5.a
  (test-case "Exercise 5.a"
    ; Positive tests:
    (check-true (lambda? (quote (lambda (x) x))))
    (check-true (lambda? (quote (lambda (x) ()))))
    (check-true (lambda? (quote (lambda () (+ 1 2)))))
    (check-true (lambda? (quote (lambda (a b c d) (+ a b c d)))))
    (check-true (lambda? (quote (lambda (x) (display x) x))))
    (check-true (lambda? (quote (lambda (x) (lambda (y) (+ x y))))))
    (check-true (lambda? (quote (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))))
    (check-true (lambda? (quote (lambda (x) (let ((y (* x 2))) (+ x y))))))
    ; Negative tests:
    (check-false (lambda? (quote ())))
    (check-false (lambda? (quote (lambda))))
    (check-false (lambda? (quote 10)))
    (check-false (lambda? (quote (define ()))))
    (check-false (lambda? (quote (lambda (1) 2))))
    (check-false (lambda? (quote 3)))
    (check-false (lambda? (quote (lambda ()))))
    (check-false (lambda? (quote (lambda x 10))))
    ; Example of using andmap
    (check-true (andmap symbol? (quote (x y z))))
    (check-false (andmap symbol? (quote (x 3 z))))
  )
)

; 5.b)
(define-test-suite test-5.b
  (test-case "Exercise 5.b"
    (check-equal? (lambda-params (quote (lambda (a b c) 1 2 3))) (quote (a b c)))
    (check-equal? (lambda-params (quote (lambda (x) y))) (quote (x)))
    (check-equal? (lambda-params (quote (lambda () (+ 1 2)))) (quote ()))
    (check-equal? (lambda-params (quote (lambda (foo bar) foo))) (quote (foo bar)))
    (check-equal? (lambda-params (quote (lambda (single) single))) (quote (single)))
    (check-equal? (lambda-params (quote (lambda (p q r s) (+ p q r s)))) (quote (p q r s)))
  )
)

; 5.c)
(define-test-suite test-5.c
  (test-case "Exercise 5.c"
    (check-equal? (lambda-body (quote (lambda (a b c) 1 2 3))) '(1 2 3))
    (check-equal? (lambda-body (quote (lambda (x) y))) (quote (y)))
    (check-equal? (lambda-body (quote (lambda () (+ 1 2)))) (quote ((+ 1 2))))
    (check-equal? (lambda-body (quote (lambda (foo bar) foo))) (quote (foo)))
    (check-equal? (lambda-body (quote (lambda (x) (+ x 10) (* x 2)))) (quote ((+ x 10) (* x 2))))
    (check-equal? (lambda-body (quote (lambda (a b) (define c 3) (+ a b c)))) (quote ((define c 3) (+ a b c))))
  )
)

; 5.d)
(define-test-suite test-5.d
  (test-case "Exercise 5.d"
    ; Positive tests:
    (check-true (apply? (quote (x y))))
    (check-true (apply? (quote ((lambda (x) x) 1))))
    (check-true (apply? (quote (f a b c))))
    (check-true (apply? (quote (+ 1 2))))
    (check-true (apply? (quote (single))))
    (check-true (apply? (quote ((lambda (a b) (+ a b)) 3 4))))
    (check-true (apply? (quote (map add1 (quote (1 2 3))))))
    ; Negative tests:
    (check-false (apply? (quote ())))
    (check-false (apply? (quote 10)))
    (check-false (apply? (quote atom)))
    (check-false (apply? (quote "")))
    (check-false (apply? (quote #t)))
    (check-false (apply? (quote #f)))
    (check-false (apply? (quote 42)))
    (check-false (apply? (quote -17)))
    (check-false (apply? (quote 3.14)))
  )
)

; 5.e)
(define-test-suite test-5.e
  (test-case "Exercise 5.e"
    (check-equal? (apply-func (quote (x y))) (quote x))
    (check-equal? (apply-func (quote (+ 1 2))) (quote +))
    (check-equal? (apply-func (quote (map add1 (quote (1 2 3))))) (quote map))
    (check-equal? (apply-func (quote (single))) (quote single))
    (check-equal? (apply-func (quote ((lambda (a b) (+ a b)) 3 4))) (quote (lambda (a b) (+ a b))))
  )
)
; 5.f)
(define-test-suite test-5.f
  (test-case "Exercise 5.f"
    (check-equal? (apply-args (quote (x y))) (quote (y)))
    (check-equal? (apply-args (quote (+ 1 2))) (quote (1 2)))
    (check-equal? (apply-args (quote (map add1 (quote (1 2 3))))) (quote (add1 (quote (1 2 3)))))
    (check-equal? (apply-args (quote (single))) (quote ()))
    (check-equal? (apply-args (quote ((lambda (a b) (+ a b)) 3 4))) (quote (3 4)))
  )
)
; 5.g)
(define-test-suite test-5.g
  (test-case "Exercise 5.g"
    (check-true (define? (quote (define x 3))))
    (check-true (define? (quote (define (f x) (+ x 1)))))
    (check-false (define? (quote (lambda (x) x))))
    (check-false (define? (quote (+ 1 2))))
    (check-false (define? (quote x)))
  )
)

; 5.h)
(define-test-suite test-5.h
  (test-case "Exercise 5.h"
    ; Positive tests
    (check-true (define-basic? (quote (define x 3))))
    (check-true (define-basic? (quote (define define 2))))
    (check-true (define-basic? (quote (define define define))))
    (check-true (define-basic? (quote (define my-var 42))))
    (check-true (define-basic? (quote (define result (+ 1 2 3)))))
    (check-true (define-basic? (quote (define empty-list null))))
    ; Negative tests
    (check-false (define-basic? (quote (define))))
    (check-false (define-basic? (quote (define 1))))
    (check-false (define-basic? (quote (define 1 2))))
    (check-false (define-basic? (quote (define () 2))))
    (check-false (define-basic? (quote (define (1) 2))))
    (check-false (define-basic? (quote (define (x) 2))))
    (check-false (define-basic? (quote (define (x 1) 2))))
    (check-false (define-basic? (quote (1 define 2))))
    (check-false (define-basic? (quote (1 2 define))))
    (check-false (define-basic? (quote (define 2 define))))
  )
)

; 5.i)
(define-test-suite test-5.i
  (test-case "Exercise 5.i"
    (check-true (define-func? (quote (define (x) 3))))
    (check-true (define-func? (quote (define (x y z x) 3 3 3 3))))
    (check-true (define-func? (quote (define (f a b c d) (+ a b c d)))))
    (check-true (define-func? (quote (define (helper x) (display x) x))))
    (check-true (define-func? (quote (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1))))))))
    (check-true (define-func? (quote (define (curry x) (lambda (y) (+ x y))))))
    ; Negative tests
    (check-false (define-func? (quote ())))
    (check-false (define-func? (quote (define))))
    (check-false (define-func? (quote 10)))
    (check-false (define-func? (quote (define x 3))))
    (check-false (define-func? (quote (lambda (x) x))))
    (check-false (define-func? (quote (define (1) 2))))
    (check-false (define-func? (quote (define () 2))))
    (check-false (define-func? (quote (define (x 1) 2))))
  )
)

(define tests
  (test-suite "Tests"
    test-4
    test-5.a
    test-5.b
    test-5.c
    test-5.d
    test-5.e
    test-5.f
    test-5.g
    test-5.h
    test-5.i
  )
)

(exit (run-tests tests 'verbose))