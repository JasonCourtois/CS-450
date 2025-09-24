#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################
|#
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1
(struct pair (left right) #:transparent)

;; Exercise 1.a
(define (pair-set-left p l)
    ;; Only keeps the old rhs
    (match p
        [(pair lhs rhs) (pair l rhs)]))

;; Exercise 1.b
(define (pair-set-right p r)
    ;; Only keeps the old lhs
    (match p
        [(pair lhs rhs) (pair lhs r)]))

;; Exercise 1.c
(define (pair-swap p)
    (match p
        [(pair lhs rhs) (pair rhs lhs)]))

;; Exercise 1.d
;; You can only use match* one time. You cannot use match.
(define (pair-add p1 p2) 
    ;; match* has one case where we have two pairs, and a new pair is made by adding the two pairs pointwise.
    (match* (p1 p2)
        [((pair lhs1 rhs1) (pair lhs2 rhs2))
        (pair (+ lhs1 lhs2) (+ rhs1 rhs2))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.a
(define (name first last)
    ;; Inside constructor for name is a lambda that takes a selection to find the first or last name.
    (lambda (selection) 
        (match selection
            ['first first]
            ['last last])))

;; Exercise 2.b
(define (first-name p) (p 'first))

;; Exercise 2.c
(define (last-name p) (p 'last))

;; Exercise 2.d
(define (full-name p)
    ;; Concatenate first name, a space, and last name utilizing the above accessors.
    (string-append (first-name p) " " (last-name p)))

;; Exercise 2.e
(define (initials p) 
    ;; Get initials by appending the substring of the first character from the first and last names.
    (string-append 
        (substring (first-name p) 0 1) 
        (substring (last-name p ) 0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3
(define (max-from n l)
    (match l
        [(list) n]
        [(list h l ...)
            (max h (max-from n l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4
(define (min-from n l)
    (match l
        [(list) n]
        [(list h l ...)
            (min h (min-from n l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5: revisit Exercise 3 and Exercise 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6
(define (count l) 'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7
(define (sum l) 'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8
(define (occurrences x l) 'todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9
(define (norm l) 'todo)

