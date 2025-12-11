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
(require "hw9-util.rkt")

(provide (all-defined-out))

(: translate-var (-> s:variable j:variable))
(define (translate-var x)
  (match x
    [(s:variable x) (j:variable x)]
  )
)

;;;;;;;;;;;;;;;
;; Exercise 1
(: translate  (-> s:expression j:expression))
(define (translate exp)
  (match exp
    [(? k:const? k) k]
    [(? s:variable? x) (translate-var x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))
    ]
    ;; (get-field (deref J[[x]])"y")
    [(s:load x y)
      (j:get
        ;; (deref J[[x]])
        (j:deref (translate x))
        ;; "y"
        (mk-field y)  
      )
    ]
    ; J[[x.y := e]]
    [(s:assign x y e)
      ; let data = J[[e]] in
      (mk-let (translate e)
        (lambda (data)
          ;  let o = (deref J[[x]]) in
          (mk-let (j:deref (translate x))
            (lambda (o)
              (j:seq
                ; (set! J[[x]] (update-field o "y" data));
                (j:assign (translate x) (j:set o (mk-field y) data))
                ; data
                data
              )
            )
          )
        )
      )
    ]
    ; J[[x.y(e ···)]]]
    [(s:invoke x y e)
      ; let m =(get-field (deref J[[x]])"y")in
      (mk-let (j:get (j:deref (translate x)) (mk-field y))
        (lambda (m)
          ; let f =(get-field (deref m)"$code")in
          (mk-let (j:get (j:deref m) (k:string "$code"))
            (lambda (f)
              ; (f J[[x]]J[[e ···]]
              (j:apply f (cons (translate x) (map translate e)))
            )
          )
        )
      )
    ]
    ; J[[function(x ···){e}]]
    [(s:function x e)
      (mk-object
        ; "$code" : λ(this, J[[x]]···).J[[e]]
        (cons "$code"
          (j:lambda
            ; this parameter first, then translated parameters
            (cons (j:variable 'this) (map translate-var x))
            ; translated body
            (translate e)
          )
        )
        ; "prototype" : (alloc {})
        (cons "prototype" (mk-object))
      )
    ]
    ; J[[new ef(e ···)]]
    [(s:new ef e)
      ; let ctor =(deref J[[ef]])in
      (mk-let (j:deref (translate ef))
        (lambda (ctor)
          ; let obj =(alloc {"$proto" :(get-field ctor "prototype")})in
          (mk-let (mk-object (cons "$proto" (j:get ctor (k:string "prototype"))))
            (lambda (obj)
              ; let f =(get-field ctor "$code")in
              (mk-let (j:get ctor (k:string "$code"))
                (lambda (f)
                  (j:seq
                    ; (f obj J[[e]]···);
                    (j:apply f (cons obj (map translate e)))
                    ; obj
                    obj
                  )
                )
              )
            )
          )
        )
      )
    ]
  )
)
