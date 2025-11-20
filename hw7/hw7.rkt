#lang typed/racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
(require "hw7-util.rkt")
(provide (all-defined-out))
;; END OF REQUIRES

;;;;;;;;;;;;;;;
;; Exercise 1
;
; Given a frame, return all handles contained therein.
;
; Use frame-values; only closure's env is relevant to this problem
; Read the hints in the PDF.
(: frame-refs (-> frame (Setof handle)))
(define (frame-refs frm)
  ;; Get all values from frame.
  (define values (frame-values frm))
  ;; Get all closures by filtering out all other items from values.
  (define closures (filter d:closure? values))
  ;; Get the handles by using map on the closures.
  (define handles (map d:closure-env closures))

  ;; Create result list by adding the parent frame handle if one is present.
  (define result
    (if (frame-parent frm)
        (cons (frame-parent frm) handles)
        handles)
  )  

  ;; Convert the result list to a set.
  (list->set result)
)

;;;;;;;;;;;;;;;
;; Exercise 2
; Standard graph algorithm: return all handles reacheable via `contained`.
; Hint: Consider solving this exercise last, as conceptually the most difficult
;       exercise.
; Hint: This is a simple breadth-first algorith. The algorithm should start
;       in env and obtain the set of next elemens by using `contained`.
; Hint: The algorithm must handle cycles.
; Hint: Do not hardcode your solution to frames (you should test it with
;       frames though)
(: mem-mark
  (All [T]
    (->
      (-> T (Setof handle))
      (heap T)
      handle
      (Setof handle)
    )
  )
)
(define (mem-mark contained mem env)

  (: mem-mark-iter (-> (Listof handle) (Setof handle) (Setof handle)))
  ; One solution to this problem is to loop while maintaining
  ; a list of environments to visit, and a set of elements visited
  (define (mem-mark-iter to-visit visited)
    (match to-visit
      [(list) visited]  ;; If there are no more items to visit, return visited.
      [(cons handle to-visit)
        (define frame (heap-get mem handle))               ;; Get frame associated with handle.
        (define c (contained frame))                       ;; Get references that the frame points to.
        (define new (set->list (set-subtract c visited)))  ;; Removed visited from c and convert to list.

        ;; Create a new list with updated nodes to visit by adding items in new.
        (define updated-to-visit (append to-visit new))
        ;; Create a new visited set by adding the handle we just visited.
        (define updated-visited (set-add visited handle))

        ;; Call the iter function with updated to-visit and visited.
        (mem-mark-iter updated-to-visit updated-visited)
      ]
    )
  )
  ; run the loop with 1 element to visit, and 1 element visited
  (mem-mark-iter (list env) (set env))
)

;;;;;;;;;;;;;;;
;; Exercise 3
;
; Return a new heap that only contains the key-values referenced in to-keep.
;
; Tip 1: We have learned a similar pattern than what is asked here.
; Tip 2: The function you want to use starts with heap-
; Tip 3: The solution is a one-liner
(: mem-sweep
  (All [T]
    (->
      ; heap
      (heap T)
      ; set of handles to keep
      (Setof handle)
      ; the new heap
      (heap T)
    )
  )
)
(define (mem-sweep mem to-keep)
  ;; Lambda function checks if handle (key) is a member of set to-keep. 
  ;; Heap-filter expects function to have two inputs (key value) even though only the key is needed.
  (heap-filter (lambda (key value) (set-member? to-keep key)) mem)
)

;;;;;;;;;;;;;;;
;; Exercise 4

;; Testing by first creating iter-map - not used in solution for eff-map.
(: iter-map (All [A B] (-> (-> A B) (Listof A) (Listof B))))
(define (iter-map f l)
  (: loop (-> (Listof B) (Listof A) (Listof B)))
  (define (loop accum l)
    (match l
      [(list) (reverse accum)]
      [(cons h l)
        (loop (cons (f h) accum) l)
      ]
    )
  )
  (loop (list) l)
)

(: eff-map
  (All [State Input Output]
    (->
      (-> Input Output)
      (Listof (eff-op State Input))
      (eff-op State (Listof Output))
    )
  )
)

(define (eff-map f l)
  (: loop 
    (->
      (Listof Output)
      (Listof (eff-op State Input))
      (eff-op State (Listof Output))
    )
  )
  (define (loop accum l)
    (match l
      [(list) (eff-pure (reverse accum))] ;; Return when map has reached end of list and reverse the accumulator
      [(cons h l)
        (eff-bind h
          (lambda ([h-value : Input])
            (loop (cons (f h-value) accum) l)))]  ;; Apply the function to h-value and add that to accumulator Then call the loop again.
    )
  )
  (loop (list) l) ;; Start iterative loop.
)

;;;;;;;;;;;;;;;
;; Exercise 5

;; Basic version of exists we covered in class - use reference https://cogumbreiro.github.io/teaching/cs450/f23/lecture21.html#5 to convert to eff.
(: exists?
  (All [T]
    (->
      (-> T Boolean)
      (Listof T)
      Boolean
    )
  )
)
(define (exists? f l)
  (match l
    [(cons h l)
      (cond 
        [(f h) #t]
        [else (exists? f l)])
    ]
    [(list) #f]
  )
)


(: eff-exists?
  (All [State T]
    (->
      (-> T Boolean)
      (Listof (eff-op State T))
      (eff-op State Boolean)
    )
  )
)
(define (eff-exists? f l)
  (match l
    [(cons h l)
      (eff-bind h
        (lambda ([h-value : T]) : (eff-op State Boolean)  ;; Adding type to output was needed to satisfy typechecker.
          (cond
            [(f h-value) (eff-pure #t)] ;; If item was found, return true.
            [else (eff-exists? f l)]))) ;; Otherwise search in the rest of the list.
    ]
    [(list) (eff-pure #f)]  ;; If list is empty, nothing was found.
  )
)

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
When reference counting something in memory, 
if the counter overflows to 0 the soundness would be impacted but not completeness.
The soundness would be impacted because the data stored at this memory was still needed.
We know that the data was still needed because the reference count should have been greater than 0.
However, the completeness would not be affected. This is because setting the reference count to 0
means that the memory will be released. Completeness only states that all unneeded data will eventually be reclaimed.
Therefore reclaiming the memory prematurely does not impact completeness.

Source: https://cogumbreiro.github.io/teaching/cs450/f23/lecture19.html#12
|#
