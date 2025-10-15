#lang errortrace typed/racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
(require "hw4-util.rkt")
(provide (all-defined-out))

(: stream-skip
  (All [Elem] ; Parameterized on the type of the elements of the stream
    (->
      ; The first parameter is the number of elements we wish to skip
      Real
      ; The input is a stream of elements
      (stream Elem)
      ; The output is a stream of elements
      (stream Elem)
    )
  )
)
(define (stream-skip n s)
  (: loop 
    (-> 
      ;; First input is Real for the counter.
      Real
      ;; Second input is the stream of elements.
      (stream Elem)
      ;; Outputs a stream-add of elements of the same type as input.
      ;; This will eventually get returned as a stream as this loop function is surrounded by a lambda with no arguments. 
      (stream-add Elem)
    )
  )
  (define (loop counter s)
    ;; Match the stream to extract the head item from rest of stream.
    (match (s)
      [(stream-add h s)
        (cond
          ;; If the counter is less than the skip item input n, return the rest of the stream and increment counter.
          [(< counter n)
            (loop (+ counter 1) s)]
          ;; Otherwise, we have skipped enough items so return a stream-add object.
          [else
            (stream-add h s)])  
      ]
    )
  )
  (lambda ()
    ;; Call the loop function with counter initialized to 0 inside a lambda.
    (loop 0 s)
  )
)


(: stream-fold
  ; We have 2 type parameters,
  ; 1. the type of elements of the stream
  ; 2. the type of the result being accumulated
  (All [Elem Accum]
    (->
      ; The type of the step function f
      (-> Elem Accum Accum)
      ; The type of the value being accumulated
      Accum
      ; The input stream of elements
      (stream Elem)
      ; The output stream of folded elements
      (stream Accum)
    )
  )
)
(define (stream-fold f a s)
  (lambda () 
    (match (s)
      [(stream-add h s)
        (define newAcc (f h a)) ;; Compute the new accumulator.
        (define result (stream-fold f newAcc s))  ;; Compute the next stream item with new accumulator.
        (stream-add a result) ;; Return a stream-add with the first item being the current accumulator, and the rest being the new steam.
      ]
    )
  )
)

(: set-void set)
(define set-void
  ;; Create a lambda that just returns the empty set when called.
  (lambda () 
    (set-empty))
)

(: set-epsilon set)
(define set-epsilon
  ;; Lambda returns a set with an empty string followed by empty set.
  (lambda () 
    (set-add "" set-void))
)

(: set-char (-> Char set))
(define (set-char x)
  ;; Lambda converts a character into a string using the string function, then makes a set with it.
  (lambda () 
    (set-add (string x) set-void))  
)


(: set-prefix (-> String set set))
(define (set-prefix s p)
  (lambda ()
    ;; Matches set with empty set and non empty set.
    (match (p)
      [(set-empty) (set-empty)] ;; Simply return (set-empty) when set is empty.
      [(set-add h p)
        ;; Create a new string by appending the prefix to the front of next item.
        (define newString (string-append s h))
        ;; Compute the result of applying prefix to the next stream.
        (define result (set-prefix s p))
        (set-add newString result)
      ]
    )  
  )
)

(: set-union (-> set set set ))
(define (set-union p1 p2)
  (lambda ()
    ;; Matches the first set input with empty and non empty cases.
    (match (p1)
      [(set-empty) (p2)]  ;; If p1 is empty, return p2
      [(set-add h p1)
        ;; result of calling set-union on the p2 and the remainder of p1.
        (define result (set-union p2 p1))
        ;; Construct set with head of p1 and the result of recursive call.
        (set-add h result)
      ]
    )
  )
)

(: set-concat (-> set set set))
(define (set-concat p1 p2)
  (lambda () 
    (match (p1)
      [(set-empty) (set-empty)] ;; If p1 is empty, return empty set.
      [(set-add h p1)
        ;; Create a set where the head of p1 is a prefix of everything from p2.
        (define concatSet (set-prefix h p2))
        ;; Get the result of set-concat on the rest of p1.
        (define result (set-concat p1 p2))
        ;; Generate the output set by taking the union of the prefix set and recursive result.
        (define outputSet (set-union concatSet result))
        ;; Call outputSet because the output of set-union is a function that returns a set.
        ;; If we didn't call outputSet, The output would be a function that returns a function that returns a set.
        ;; This removes the middle function call.
        (outputSet)
      ]  
    )
  )
)      

(: r:eval-exp (-> r:expression Number))
(define (r:eval-exp exp)
  (match exp
    ; If it's a number, return that number.
    [(r:number v) v]
    ; Case for applying a function.
    [(r:apply (r:variable f) l)
      (define func (r:eval-builtin f))  ;; Get the specified function.
      (define evaluatedList (map r:eval-exp l)) ;; Use map to compute eval-exp on every item in the list of arguments.
      (apply func evaluatedList)  ;; Use apply to execute the function with the list of arguments.
    ]
  )
)

(: r:exp-to-string (-> r:expression String))
(define (r:exp-to-string exp)
  (match exp
    ;; Cases for r:number and r:variable both just return a string representation of the number or variable.
    [(r:number v) (format "~a" v)]
    [(r:variable v) (format "~a" v)]
    [(r:apply f l)
      ;; evaluated list has a string of the function call followed by string representations of each argument.
      (define evaluatedList (cons (r:exp-to-string f) (map r:exp-to-string l)))
      ;; Format handles adding parenthesis around the entire list, which represents the function call, and putting a space between each element.
      (format "~a" evaluatedList)]
  )
)

