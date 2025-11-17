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
(require "hw7-util.rkt")
(require "hw7-parse.rkt")
(require "hw7.rkt")

(require rackunit/text-ui)

(define (pop)
  (lambda (stack)
    (eff (rest stack) (first stack))
  )
)
(define (push n)
  (lambda (stack)
    (eff (cons n stack) (void))
  )
)

(define (inc)
  (lambda (state)
    (eff (+ state 1) state)
  )
)

(define (dec)
  (lambda (state)
    (eff (- state 1) state)
  )
)

(define (store n)
  (lambda (state)
    (eff n state)
  )
)
(define (get)
  (lambda (state)
    (eff state state)
  )
)
(define (+= x)
  (lambda (state)
    (eff (+ state x) state)
  )
)
(define (*= x)
  (lambda (state)
    (eff (* state x) state)
  )
)

(define-test-suite test-1
  (test-case "Exercise 1"

    (check-equal? (frame-refs (parse-frame '[(x . 2) (y . 10) (z . 0)]))
      (set))
    (check-equal? (frame-refs (parse-frame '[E10 (x . 2) (y . 10) (z . 0)]))
      (set (handle 10)))

    (check-equal? (frame-refs (parse-frame '[(x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
      (set (handle 0) (handle 1)))

    (check-equal? (frame-refs (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
      (set (handle 0) (handle 1) (handle 9)))
  )
)

(define-test-suite test-2
  ;;;;;;;;;;;;;;;
  ;; Exercise 2
  (test-case "Exercise 2"
    (define m1
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])
          (E2 . [E0 (x . 10)])
          (E3 . [E0 (x . 5)])]))
    (check-equal? (mem-mark frame-refs m1 (handle 0)) (set (handle 0)))
    (check-equal? (mem-mark frame-refs m1 (handle 1)) (set (handle 0) (handle 1)))
    (check-equal? (mem-mark frame-refs m1 (handle 2)) (set (handle 0) (handle 2)))
    (check-equal? (mem-mark frame-refs m1 (handle 3)) (set (handle 0) (handle 3)))


    (define m2
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])
          (E2 . [E1 (x . 10)])
          (E3 . [E3 (a . 5) (b . (closure E0 (lambda (x) (lambda (y) x)))) (c . (closure E1 (lambda (x) (lambda (y) x))))])
          (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])]))
    (check-equal? (mem-mark frame-refs m2 (handle 0)) (set (handle 0)))
    (check-equal? (mem-mark frame-refs m2 (handle 1)) (set (handle 0) (handle 1)))
    (check-equal? (mem-mark frame-refs m2 (handle 2)) (set (handle 0) (handle 1) (handle 2)))
    (check-equal? (mem-mark frame-refs m2 (handle 3)) (set (handle 0) (handle 1) (handle 3)))
    (check-equal? (mem-mark frame-refs m2 (handle 4)) (set (handle 0) (handle 1) (handle 3) (handle 4)))

    (define m3
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E1 (x . 2)])
          (E2 . [E1 (x . 10)])
          (E3 . [E3 (a . 5) (b . (closure E3 (lambda (x) (lambda (y) x)))) (c . (closure E0 (lambda (x) (lambda (y) x))))])
          (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])]))

    (check-equal? (mem-mark frame-refs m3 (handle 0)) (set (handle 0)))
    (check-equal? (mem-mark frame-refs m3 (handle 1)) (set (handle 1)))
    (check-equal? (mem-mark frame-refs m3 (handle 2)) (set (handle 1) (handle 2)))
    (check-equal? (mem-mark frame-refs m3 (handle 3)) (set (handle 0) (handle 3)))
    (check-equal? (mem-mark frame-refs m3 (handle 4)) (set (handle 0) (handle 1) (handle 3) (handle 4)))


    (define m6
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])
          (E2 . [E1 (x . 10)])
          (E3 . [E3 (a . 5) (b . (closure E0 (lambda (x) (lambda (y) x)))) (c . (closure E1 (lambda (x) (lambda (y) x))))])
          (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])
          (E5 . [E2 (x . (closure E0 (lambda (x) x)))])
          (E6 . [E5 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E2 (lambda (z) z)))])]))

    (check-equal? (mem-mark frame-refs m6 (handle 0)) (set (handle 0)))
    (check-equal? (mem-mark frame-refs m6 (handle 1)) (set (handle 0) (handle 1)))
    (check-equal? (mem-mark frame-refs m6 (handle 2)) (set (handle 0) (handle 1) (handle 2)))
    (check-equal? (mem-mark frame-refs m6 (handle 3)) (set (handle 0) (handle 1) (handle 3)))
    (check-equal? (mem-mark frame-refs m6 (handle 4)) (set (handle 0) (handle 1) (handle 3) (handle 4)))
    (check-equal? (mem-mark frame-refs m6 (handle 5)) (set (handle 0) (handle 1) (handle 2) (handle 5)))
    (check-equal? (mem-mark frame-refs m6 (handle 6)) (set (handle 0) (handle 1) (handle 2) (handle 5) (handle 6)))


    (define m7
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E5 (x . 2)])
          (E2 . [E1 (x . 10)])
          (E3 . [E3 (a . 5) (b . (closure E0 (lambda (x) (lambda (y) x)))) (c . (closure E1 (lambda (x) (lambda (y) x))))])
          (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])
          (E5 . [E2 (x . (closure E0 (lambda (x) x)))])
          (E6 . [E0 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E2 (lambda (z) z)))])]))

    (check-equal? (mem-mark frame-refs m7 (handle 6)) (set (handle 0) (handle 1) (handle 2) (handle 5) (handle 6)))
  )
)

(define-test-suite test-3
  ;;;;;;;;;;;;;;;
  ;; Exercise 3
  (test-case "Exercise 3"

    (define m2
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])
          (E2 . [E1 (x . 10)])
          (E3 . [E3 (a . 5) (b . (closure E0 (lambda (x) (lambda (y) x)))) (c . (closure E1 (lambda (x) (lambda (y) x))))])
          (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])]))

    (check-equal? (mem-sweep m2 (set (handle 0)))
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])]))
    (check-equal? (mem-sweep m2 (set (handle 0) (handle 1)))
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])]))
    (check-equal? (mem-sweep m2 (set (handle 0) (handle 1)))
      (parse-mem
        '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
          (E1 . [E0 (x . 2)])]))
  )
)

(define-test-suite test-4
  ;;;;;;;;;;;;;;;
  ;; Exercise 4
  (test-case "Exercise 4"
    (check-equal?
      (
        (eff-map (lambda (x) x) (list (inc) (inc) (inc) (inc)))
        ; Set the initial state to 0
        0
      )
      (eff
        4 ; The final state
        '(
          0 ; the result of the first inc
          1 ; the result of the second inc
          2 ; the result of the third inc
          3 ; the result of the fourth inc
        )
      )
    )
    (check-equal?
      (
        (eff-map
          ; Our map simply returns the values unchanged
          (lambda (x) x)
          (list
            ; state = 0
            (store 10) ; returns 0
            ; state = 10
            (inc) ; returns 10
            ; state = 11
            (dec) ; returns 11
            ; state = 10
            (dec) ; returns 10
            ; state = 9
            (store 99) ; returns 9
            ; state = 99
            (store 0) ; returns 99
            ; state = 0
            (inc) ; return 0
            ; state = 1
          )
        )
        ; Set the initial state to 0
        0
      )
      (eff
        1 ; The final state
        '(0 10 11 10 9 99 0) ; this list holds the various **returns**
      )
    )
    (define l (range 10))
    (check-equal?
      (
        (eff-map (lambda (x) x) (map push l))
        ; initial state
        (list)
      )
      (eff (reverse l) (map (lambda (x) (void)) l))
    )
    (check-equal?
      (
        (eff-map
          (lambda (x) x)
          (list
            (+= 2)
            (*= 2)
            (+= 3)
            (*= 3)
            (+= -1)
            (*= -1)
          )
        )
        0
      )
      (eff -20 '(0 2 4 7 21 20))
    )

  )
)
(define-test-suite test-5
  ;;;;;;;;;;;;;;;
  ;; Exercise 5
  (test-case "Exercise 5"
    (check-equal?
      ; eff-exists? only cares about the results being returned, not about the internal state!

      (
        (eff-exists?
          ; evaluates until the first **result** is 9
          (lambda (x) (equal? x 9))
          (list
            ; state = 0
            (store 10) ; returns 0
            ; state = 10
            (inc) ; returns 10
            ; state = 11
            (dec) ; returns 11
            ; state = 10
            (dec) ; returns 10
            ; state = 9
            (store 99) ; returns 9
            ; state = 99
            (store 0) ; returns 99
            ; state = 0
            (inc) ; return 0
            ; state = 1
          )
        )
        ; Initial state = 0
        0
      )
      ; note that (store 0) and (inc) cannot execute, since
      ; we only evaluate until a result equals 9
      (eff 99 #t)
    )
    (check-equal?
      (
        (eff-exists?
          (lambda (x) #f)
          (list (inc) (inc) (inc) (inc))
        )
        ; Set the initial state to 0
        0
      )
      (eff
        4 ; The final state
        #f
      )
    )
    (check-equal?
      (
        (eff-exists?
          (lambda (x) (equal? x 7))
          (list
            (+= 2)
            (*= 2)
            (+= 3)
            (*= 3)
            (+= -1)
            (*= -1)
          )
        )
        0
      )
      (eff 21 #t)
    )

    (define l (range 100))
    (check-equal?
      (
        (eff-exists? (lambda (x) #f) (map push l))
        ; initial state
        (list)
      )
      (eff (reverse l) #f)
    )
  )
)


(define tests
  (test-suite "Tests"
    test-1
    test-2
    test-3
    test-4
    test-5
  )
)


; Run the tests
(run-tests tests 'verbose)
