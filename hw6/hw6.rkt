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
      (error "todo!")
      ; Return: E(x) ▶ H
    ]
    [(d:lambda x t) ; H is mem
      (error "todo!")
      ; Return: {E, λx.t} ▶ H
    ]
    [(d:apply ef ea) ; H1 is mem
      (match (eval-exp mem env ef)
        ;; ef ⇓E {Ef, λx.tb} ▶ H2
        [(eff H2 (d:closure Ef x tb))
          ;; ea ⇓E va ▶ H3
          ;; ...
          ;; Eb ← Ef + [x := va] ▶ H4
          ;; ...
          ;; tb ⇓Eb vb ▶ H5
          ;; ...
          (error "todo!")
          ;; Return: vb ▶ H5
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
      ;; ...
      ;; E ← [x := v] ▶ H3
      ;; ...
      (error "todo!")
      ;; return: void ▶ H3
    ]
    [(d:seq t1 t2) ; mem is H1
      ;; ​t1​ ⇓E ​v1 ▶ H2
      ;; ...
      ;; t2 ⇓E v2 ▶ H3
      ;; ...
      (error "todo!")
      ;; return: v2 ▶ H3
    ]
    [(? d:expression?) (eval-exp mem env term)]
  )
)

;; Exercise 3 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOUR ANSWER.
YOU MAY USE MULTIPLE LINES.
|#
