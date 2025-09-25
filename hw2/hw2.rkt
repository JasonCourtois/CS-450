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
;; Exercise 5: Reusable function for Exercises 3 and 4.
;; Comparison will either be max or min which are input in exercises 3 and 4.
(define (extreme-from comparison n l)
    (match l
        [(list) n]  ;; Base case, if we have an empty list the extreme is n
        [(list h l ...)
            (comparison h (extreme-from comparison n l))])) ;; Otherwise, compare h with the extreme result from the rest of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3
(define (max-from n l) (extreme-from max n l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4
(define (min-from n l) (extreme-from min n l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6
(define (count l)
    (match l
        [(list) 0]  ;; Base case, list empty therefore there are 0 items.
        [(list h l ...)
            (+ 1 (count l))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7
(define (sum l)
    (match l
        [(list) 0]  ;; Base case, list empty therefore add 0 to the summation.
        [(list h l ...)
            (+ h (sum l))]))    ;; Otherwise, add the value of current item to the sum of the rest of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8
(define (occurrences x l)
    (match l
        [(list) 0]  ;; Base case, list empty therefore add 0 to number of occurrences.
        [(list h l ...)
            (cond
                [(= x h) (+ 1 (occurrences x l))]   ;; If current item h is equal to x, add 1 to the rest of the occurrences.
                [else (occurrences x l)])]))    ;; Otherwise, return the number of occurrences in the rest of the list.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9

;; Helper function to square an input
(define (square x) (* x x))

(define (norm l)
    ;; Square every element in list with map, then use the above sum pattern matching to add all items, then take the square root of the sum.
    (sqrt (sum (map square l))))