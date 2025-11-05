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
#lang typed/racket
(require "hw6-util.rkt")
(provide (all-defined-out))
;; END OF REQUIRES

;; Exercise 1
(: eval-exp (-> memory handle d:expression (eff memory d:value)))
(define (eval-exp mem env exp)
  ; mem is M
  ; env is E
  (match exp
    [(? d:value?) ; H is mem and v is exp
      ; Return: v ▶ H
      (eff mem exp)
      ]
    [(? d:variable?) ; exp is x  and H is mem
      ; Return: E(x) ▶ H
      (eff mem (environ-get mem env exp))
    ]
    [(d:lambda x t) ; H is mem
      ; Return: {E, λx.t} ▶ H
      (eff mem (d:closure env x t))
    ]
    [(d:apply ef ea) ; H1 is mem
      (match (eval-exp mem env ef)
        ;; ef ⇓E {Ef, λx.tb} ▶ H2
        [(eff H2 (d:closure Ef x tb))
          ;; ea ⇓E va ▶ H3
          (define va+H3 (eval-exp H2 env ea))
          (define H3 (eff-state va+H3))
          (define va (eff-result va+H3))
          ;; Eb ← Ef + [x := va] ▶ H4
          (define Eb+H4 (environ-push H3 Ef x va))
          (define Eb (eff-result Eb+H4))
          (define H4 (eff-state Eb+H4))
          ;; tb ⇓Eb vb ▶ H5
          (define vb+H5 (eval-term H4 Eb tb))
          (define H5 (eff-state vb+H5))
          (define vb (eff-result vb+H5))
          ;; Return: vb ▶ H5
          (eff H5 vb)
        ]
      )
    ]
  )
)

;; Exercise 2

(: eval-term (-> memory handle d:term (eff memory d:value)))
(define (eval-term mem env term)
  (match term
    [(d:define x e) ; mem is H1
      ;; e ⇓E v ▶ H2
      (define v+H2 (eval-exp mem env e))
      (define H2 (eff-state v+H2))
      (define v (eff-result v+H2))
      ;; E ← [x := v] ▶ H3
      (define H3 (environ-put H2 env x v))
      ;; return: void ▶ H3
      (eff H3 (d:void))
    ]
    [(d:seq t1 t2) ; mem is H1
      ;; ​t1​ ⇓E ​v1 ▶ H2
      (define v1+H2 (eval-term mem env t1))
      (define H2 (eff-state v1+H2))
      (define v1 (eff-result v1+H2))
      ;; t2 ⇓E v2 ▶ H3
      (define v2+H3 (eval-term H2 env t2))
      (define H3 (eff-state v2+H3))
      (define v2 (eff-result v2+H3))
      ;; return: v2 ▶ H3
      (eff H3 v2)
    ]
    [(? d:expression?) (eval-exp mem env term)]
  )
)

;; Exercise 3 (Manually graded)
#|
One difference between λD and Racket can be seen in the following example code:

(define a 5)
(define a 100)
a

In Racket, this program would give an error saying "identifier already defined". However, if we
evaluate this code with λD using the following test case we get a different result:

(eval-seq*?
      ; Input memory
      '[(E0)]
      ; Environment
      'E0
      ; Input sequence
      '[
        (define a 5)
        (define a 100)
        a
      ]
      100
      '[(E0 (a . 100))])

The code evaluates the variable a to be 100. This is because in λD, we allow variables 
to be overwritten. When variable a is put into the environment, it will overwrite whatever
was previously set for variable a. However, Racket does not allow variables to be redefined.
I used the above test case inside of the hw6-test.rkt file and confirmed the output and behavior.
|#
