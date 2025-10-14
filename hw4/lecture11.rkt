#lang typed/racket

(provide (all-defined-out))

(define-type (stream T)
  (-> (stream-add T))
)

(struct [T] stream-add (
  [first : T]
  [rest : (stream T)]
  )
  #:transparent
)

(: tabs (-> Real String))
(define (tabs n)
  (if (>= n 1)
    (string-append "  " (tabs (- n 1)))
    ""
  )
)

(: naturals-of (-> Real (stream Real)))
(define (naturals-of n)
  (displayln (format "~a(naturals-of ~a)" (tabs n) n))
  (lambda ()
    (displayln (format "~ainside lambda: ~a" (tabs n) n))
    (stream-add
      n
      (naturals-of (+ n 1))
    )
  )
)

(: naturals (stream Real))
(define naturals (naturals-of 1))
;(displayln "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
;(displayln (naturals))

(: print-list (All [T] (-> Real (Listof T) Void)))
(define (print-list n l)
  (: loop (-> Real (Listof T) Void))
  (define (loop counter l)
    (match l
      [(list) (displayln "list empty")
      ]
      [(cons h l)
        (cond
          [(<= counter 0) (displayln "counter=0")]
          [else
            (displayln (format "-> ~a" h))
            (loop (- counter 1) l)
          ])
      ]
    )
  )
  (loop n l)
)
;(print-list 4 (list "a" "b" "c" "d"))



(: print-stream (All [T] (-> Real (stream T) Void)))
(define (print-stream n s)
  (: loop (-> Real (stream T) Void))
  (define (loop counter s)
    (match (s)
      [(stream-add h s)
        (cond
          [(<= counter 0) (displayln "counter=0")]
          [else
            (displayln (format "-> ~a" h))
            (loop (- counter 1) s)
          ])
      ]
    )
  )
  (loop n s)
)
; (print-stream 4 naturals)

(: map (All [A B] (-> (-> A B) (Listof A) (Listof B))))
(define (map f l)
  (match l
    [(list) (list)]
    [(cons h l)
      (define result (map f l))
      (cons (f h) result)
    ]
  )
)

(: stream-map (All [A B] (-> (-> A B) (stream A) (stream B))))
(define (stream-map f s)
  (lambda ()
    (match (s)
      [(stream-add h s)
        (define result (stream-map f s))
        (stream-add (f h) result)
      ]
    )
  )
)

;(map (lambda ([x : Real]) (expt 2 x)) (list 1 2 3 4 5))

(print-stream 10
  (stream-map (lambda ([x : Real]) (expt 2 x)) naturals)
)
;(: powers-of-two (Stream Real))
;(define (


















