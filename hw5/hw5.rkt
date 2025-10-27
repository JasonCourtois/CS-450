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
(provide (all-defined-out))
(require "hw5-util.rkt")
;; END OF REQUIRES

;; Exercise 1
(: s:subst (-> s:expression s:variable s:value s:expression))
(define (s:subst exp var val)
  (match exp
    [(? s:number?) (error "to do")]
    [(? s:variable?) (equal? exp var) (error "to do")]
    [(s:lambda x e)
      (equal? x var)
      (error "to do")]
    [(s:apply ef ea)
      (define new-ef (s:subst ef var val))
      (define new-ea (s:subst ea var val))
      (s:apply new-ef new-ea)]
  )
)

;; Exercise 2
(: s:eval (-> (-> s:expression s:variable s:value s:expression) s:expression s:value))
(define (s:eval subst exp)
  (match exp
    ; E-val
    [(? s:value?)
      (error "to do")
    ]
    ; E-app
    [(s:apply ef ea)
      ; ef ⇓ λx.eb
      (match (s:eval subst ef)
        [(s:lambda x eb)
          ; ea ⇓ va
          (define va (s:eval subst ea))
          ; eb [x ⟼ va] ⇓ vb
          ; (define vb (s:eval subst ...))
          ; return vb
          (error "to do")
        ]
      )
    ]
)

)

;; Exercise 3
(: e:eval (-> e:environ e:expression e:value))
(define (e:eval env exp)
  (match exp
    ; v ⇓E v (E-val)

    ; x ⇓E E(x) (E-var)
    [(? e:variable?)
      ; exp is x
      (error "to do")
    ]
    ; λx.t ⇓E {E, λx.t} (E-clos)

    ; (ef ea) ⇓E vb (E-apply)
    [(e:apply ef ea)
      ; ef ⇓ {Eb, λx.eb}
      (match (e:eval env ef)
        [(e:closure Eb x eb)
          ; ea ⇓E va
          ; ...
          ; eb ⇓Eb[x ⟼ va] vb
          ; ...
          ; return vb
          (error "todo")
        ]
      )
    ]
  )
)

;; Exercise 4 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOUR ANSWER.
YOU MAY USE MULTIPLE LINES.
|#

;; Exercise 5 (Manually graded)
#|
PLEASE REPLACE THIS TEXT BY YOUR ANSWER
YOU MAY USE MULTIPLE LINES.
|#
