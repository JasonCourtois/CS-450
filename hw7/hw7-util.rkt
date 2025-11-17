#lang typed/racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THIS FILE <===

  You are encouraged to read through the file for educational purposes,
  but you should not make this file available to a 3rd-party, e.g.,
  by making the file available in a website.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The heap data-structure
(provide (all-defined-out))


(struct handle ([id : Number]) #:transparent)
(struct [T] heap ([data : (Immutable-HashTable handle T)]) #:transparent)
(struct [S T] eff ([state : S] [result : T]) #:transparent)

(: heap-alloc (All [T] (-> (heap T) T (eff (heap T) handle))))
(define (heap-alloc h v)
  (define data (heap-data h))
  (define new-id (handle (hash-count data)))
  (define new-heap (heap (hash-set data new-id v)))
  (eff new-heap new-id))

(: heap-get (All [T] (-> (heap T) handle T)))
(define (heap-get h k)
  (hash-ref (heap-data h) k))

(: heap-put (All [T] (-> (heap T) handle T (heap T))))
(define (heap-put h k v)
  (define data (heap-data h))
  (cond
    [(hash-has-key? data k) (heap (hash-set data k v))]
    [else (error "Unknown handle!")]))

(: heap-filter
  (All [T]
    (->
      ; for each key val returns a boolean
      (-> handle T Boolean)
      ; Given a heap
      (heap T)
      ; Returns a heap
      (heap T)
    )
  )
)
(define (heap-filter proc hp)
  (heap
    (make-immutable-hash
      (filter
        (lambda ([pair : (Pairof handle T)]) (proc (car pair) (cdr pair)))
        (hash->list (heap-data hp))))))

;; Values
(define-type d:value (U d:void d:number d:closure))
(struct d:void () #:transparent)
(struct d:number ([value : Number]) #:transparent)
(struct d:closure (
    [env : handle]
    [param : d:variable]
    [body : d:term]
  )
  #:transparent
)

(define-type d:expression (U d:value d:variable d:apply d:lambda))
(struct d:lambda ([param : d:variable] [body : d:term]) #:transparent)
(struct d:variable ([name : Symbol]) #:transparent)
(struct d:apply ([func : d:expression] [arg : d:expression]) #:transparent)

;; Terms
(define-type d:term (U d:expression d:define d:seq))
(struct d:define ([var : d:variable] [body : d:expression]) #:transparent)
(struct d:seq ([fst : d:term] [snd : d:term]) #:transparent)


(struct frame (
    [parent : (Option handle)]
    [locals : (Immutable-HashTable d:variable d:value)]
  ) #:transparent
)

(: frame-keys (-> frame (Listof d:variable)))
(define (frame-keys frm)
  (map (lambda ([x : (Pairof d:variable d:value)]) (car x)) (hash->list (frame-locals frm))))

(: frame-values (-> frame (Listof d:value)))
(define (frame-values frm)
  (map (lambda ([x : (Pairof d:variable d:value)]) (cdr x))
    (hash->list (frame-locals frm))))


(define-type (eff-op State Result) (-> State (eff State Result)))
(: eff-bind
  (All [State Input Output]
    (->
      (eff-op State Input)
      (-> Input (eff-op State Output))
      (eff-op State Output)
    )
  )
)
(define (eff-bind o1 o2)
  (lambda ([h1 : State])
    (define h2+r (o1 h1))
    (define r (eff-result h2+r))
    (define h2 (eff-state h2+r))
    ((o2 r) h2)))

(: eff-pure
  (All [State T]
    (-> T (eff-op State T))
  )
)
(define (eff-pure x)
  (lambda (h) (eff h x)))
