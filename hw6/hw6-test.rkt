#lang errortrace racket
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

(require "hw6-util.rkt")
(require "hw6-parse.rkt")
(require "hw6.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Testing API

(define-check (eval-exp*? mem env exp expected-val expected-mem)
  (define given-mem (parse-mem mem))
  (define r (eval-exp given-mem (parse-handle env) (parse-term exp)))
  (with-check-info
    [('input-mem mem)
     ('input-env env)
     ('input-exp exp)]
    (check-equal? (quote-term (eff-result r)) expected-val "Value returned by eval-exp differs!")
    (check-equal? (quote-mem (eff-state r)) expected-mem "Memory returned by eval-exp differs!"))
)

(define-check (eval-seq*? mem env term expected-val expected-mem)
  (define given-mem (parse-mem mem))
  (define r (eval-term given-mem (parse-handle env) (parse-seq term)))
  (with-check-info
    [('input-mem mem)
     ('input-env env)
     ('input-sequence term)]
    (check-equal? (quote-term (eff-result r)) expected-val "Value returned by sequence of terms differs!")
    (check-equal? (quote-mem (eff-state r)) expected-mem "Memory returned by sequence of terms differs!")
  )
)

(define-check (eval-term*? mem env term expected-val expected-mem)
  (define given-mem (parse-mem mem))
  (define r (eval-term given-mem (parse-handle env) (parse-term term)))
  (with-check-info
    [('input-mem mem)
     ('input-env env)
     ('input-exp term)]
    (check-equal? (quote-term (eff-result r)) expected-val "Value returned by term differs!")
    (check-equal? (quote-mem (eff-state r)) expected-mem "Memory returned by term differs!")
  )
)

;; End of testing API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test-suite test-1
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Values
  (test-case "Exercise 1. values"
    ;; Number
    (eval-exp*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      1
      ; Output value
      1
      ; Output memory
      '[(E0)]
    )

    ;; Void
    (eval-exp*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '(void)
      ; Output value
      '(void)
      ; Output memory
      '[(E0)]
    )

    ;; Closure
    (eval-exp*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '(closure E0 (lambda (x) (define a 20) (define b 10) (f b)))
      ; Output value
      '(closure E0 (lambda (x) (define a 20) (define b 10) (f b)))
      ; Output memory
      '[(E0)]
    )
  )
)
(define-test-suite test-2
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Variable
  (test-case "Exercise 1. variable"
      (define m
        '[
          (E0 . [(a . 10) (x . 0)])
          (E1 . [E0 (b . 20) (x . 1)])
          (E2 . [E0 (a . 30)])
          (E3 . [E2 (z . 3)])])
      (eval-exp*? m 'E0 'x 0 m)
      (eval-exp*? m 'E0 'a 10 m)
      (eval-exp*? m 'E1 'x 1 m)
      (eval-exp*? m 'E1 'b 20 m)
      (eval-exp*? m 'E1 'a 10 m)
      (eval-exp*? m 'E2 'x 0 m)
      (eval-exp*? m 'E2 'a 30 m)
      (eval-exp*? m 'E3 'z 3 m)
      (eval-exp*? m 'E3 'a 30 m)
      (eval-exp*? m 'E3 'x 0 m)
  )
)

(define-test-suite test-3
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Lambda
  (test-case "Exercise 1. lambda"
    ; Lambda
    (eval-exp*?
      '[(E0)]
      'E0
      '(lambda (x) (define a 20) (define b 10) (f b))
      '(closure E0 (lambda (x) (define a 20) (define b 10) (f b)))
      '[(E0)]
    )
    ; Make sure the environment is not hardcoded
    (eval-exp*?
      '[(E0) (E1 E0)]
      'E1
      '(lambda (x) (define a 20) (define b 10) (f b))
      '(closure E1 (lambda (x) (define a 20) (define b 10) (f b)))
      '[(E0) (E1 E0)]
    )
    ; Same as first example, but with a different memory
    (eval-exp*?
      ; Input memory
      '[(E0 . [(x . 2)])]
      ; Environment
      'E0
      ; Input expression
      '(lambda (x) x)
      ; Output value
      '(closure E0 (lambda (x) x))
      ; Output memory
      '[(E0 . [(x . 2)])]
    )
  )
)

(define-test-suite test-4
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Variable
  (test-case "Exercise 1. lambda"
    (eval-exp*?
      '[(E0 (x . 10))]
      'E0
      'x
      10
      '[(E0 (x . 10))]
    )

    ; Lookup parent variable
    (eval-exp*?
      '[(E0 (x . 10)) (E1 E0)]
      'E1
      'x
      10
      '[(E0 (x . 10)) (E1 E0)]
    )

    ; Parent variable shadowed
    (eval-exp*?
      '[(E0 (x . 10)) (E1 E0 (x . 20))]
      'E1
      'x
      20
      '[(E0 (x . 10)) (E1 E0 (x . 20))]
    )

  )
)

(define-test-suite test-5
  ;;;;;;;;;;;;;;;;;;;;;;;
  ; Define
  (test-case "Exercise 2. define"

    ;; Define
    (eval-term*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '(define a 10)
      ; Output value
      '(void)
      ; Output memory
      '[(E0 (a . 10))])

    ;; Define
    (eval-term*?
      ; Input memory
      '[(E0) (E1 E0)]
      ; Environment
      'E1
      ; Input term
      '(define a 10)
      ; Output value
      '(void)
      ; Output memory
      '[(E0) (E1 E0 (a . 10))])


    ;; Define
    (eval-term*?
      ; Input memory
      '[(E0) (E1 E0)]
      ; Environment
      'E0
      ; Input term
      '(define a 10)
      ; Output value
      '(void)
      ; Output memory
      '[(E0 (a . 10)) (E1 E0)])

    ;; In this semantics we can overwrite any value
    (eval-term*?
      ; Input memory
      '[(E0 (a . 10)) (E1 E0)]
      ; Environment
      'E0
      ; Input term
      '(define a 20)
      ; Output value
      '(void)
      ; Output memory
      '[(E0 (a . 20)) (E1 E0)])

    (eval-term*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '(define (f x) a)
      ; Output value
      '(void)
      ; Output memory
      '[(E0 (f . (closure E0 (lambda (x) a))))]
    )

    (eval-term*?
      ; Input memory
      '[(E0 . [(x . 2)])]
      ; Environment
      'E0
      ; Input term
      '(define y 20)
      ; Output value
      '(void)
      ; Output memory
      '[(E0 . [(x . 2) (y . 20)])]
    )
  )
)

(define-test-suite test-6
  ;;;;;;;;;;;;;;;;;;;;;;
  ;; Sequence
  (test-case "Exercise 2. sequence"
    ; Make sure you solve numbers first
    (eval-seq*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '[
        1
        2
        3
        4
        5
      ]
      ; Output value
      5
      ; Output memory
      '[(E0)]
    )


    ; Make sure you solve define, numbers, and variables first:

    (eval-seq*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '[
        (define x 10)
        x
      ]
      ; Output value
      10
      ; Output memory
      '[(E0 (x . 10))]
    )

    (eval-seq*?
      ; Input memory
      '[(E0 . [])]
      ; Environment
      'E0
      ; Input term
      '[
        (define x 2)
        (define y 20)
        x
      ]
      ; Output value
      2
      ; Output memory
      '[(E0 . [(x . 2) (y . 20)])])
  )
)

(define-test-suite test-7
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Apply
  (test-case "Exercise 1. apply"
    ; The most complicated part is usually function calls
    ; Make sure you solve closure and number first
    (eval-exp*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input term
      '(
        (closure E0 (lambda (x) x))
        10
      )
      ; Output value
      10
      ; Output memory
      '[(E0)
        ; Creates a new environment that extends the environment
        ; of the closure with x:=10
        (E1 E0  (x . 10))]
    )
    ; Similar to first example, but extends a different environment
    ; Depends on: closure, number
    (eval-exp*?
      ; Input memory
      '[(E0) (E1 E0 (x . 10))]
      ; Environment
      'E0
      ; Input term
      '(
        (closure E1 (lambda (y) x))
        20
      )
      ; Output value
      10
      ; Output memory
      '[(E0)
        (E1 E0  (x . 10))
        ; Creates a new environment that extends the environment
        ; of the closure with x:=10
        (E2 E1 (y . 20))
      ]
    )
    ; Similar to second example, but loads closure from variable
    ; Depends on: closure, number, variable
    (eval-exp*?
      ; Input memory
      '[(E0 (f . (closure E1 (lambda (y) x)))) (E1 E0 (x . 10))]
      ; Environment
      'E0
      ; Input term
      '(f 20)
      ; Output value
      10
      ; Output memory
      '[(E0 (f . (closure E1 (lambda (y) x))))
        (E1 E0  (x . 10))
        ; Creates a new environment that extends the environment
        ; of the closure with x:=10
        (E2 E1 (y . 20))
      ]
    )

    ; Depends on: lambda, closure, variable, number
    (eval-exp*?
      ; Input memory
      '[(E0 (x . 10)) (E1 E0) (E2 E1) (E3 E2)]
      ; Environment
      'E0
      ; Input term
      '((closure E3 (lambda (z) x)) 3)
      ; Output value
      10
      ; Output memory
      '[
        (E0 (x . 10))
        (E1 E0)
        (E2 E1)
        (E3 E2)
        (E4 E3 (z . 3))
      ]
    )
  )
)

(define-test-suite test-8
  (test-case "Exercise 1+2. full examples"

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Full examples
    ; Example done in class
    ; Depends on: define, lambda, variable, closure, sequence, apply
    (eval-seq*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input sequence
      '[
        (define b (lambda (x) a))
        (define a 20)
        (b 1)
      ]
      20
      '[(E0 (a . 20) (b closure E0 (lambda (x) a))) (E1 E0 (x . 1))]
    )

    ; Example done in class
    ; Depends on: define, lambda, variable, closure, sequence, apply
    (eval-seq*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input sequence
      '[
        (define a 20)
        (define b (lambda (x) a))
        (b 1)
      ]
      20
      '[(E0 (a . 20) (b closure E0 (lambda (x) a))) (E1 E0 (x . 1))]
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
    test-6
    test-7
    test-8
  )
)


; Run the tests
(run-tests tests 'verbose)
