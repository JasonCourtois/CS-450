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
    [(? s:number?) exp] ;; The expression is just a number, so return it. n[x↦v]=n
    [(? s:variable?) 
      (cond
        [(equal? exp var) val]  ;; Substitute variable with value. x[x↦v]=v
        [else exp])]            ;; Don't substitute because variables are different. y[x↦v]=y if x!=y
    [(s:lambda x e)
      (cond
        [(equal? x var) exp]  ;; If variable in lambda matches variable being substituted, don't substitute. (λx.e)[x↦v]=λx.e
        [else 
          (define result-expression (s:subst e var val))
          (s:lambda x result-expression)])] ;; If variable in lambda is different, then substitute. (λy.e)[x↦v]=λy.(e[x↦v]) if x!=y
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
      exp ;; Simply return the value stored in exp.
    ]
    ; E-app
    [(s:apply ef ea)
      ; ef ⇓ λx.eb
      (match (s:eval subst ef)
        [(s:lambda x eb)
          ; ea ⇓ va
          (define va (s:eval subst ea))
          ; eb [x ⟼ va] ⇓ vb
          (define vb (s:eval subst (s:subst eb x va)))  ;; Substitute variable x with value va into lambda body, then evaluate.
          ; return vb
          vb
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
    [(? e:value?)
      exp ;; Simply return the value stored in exp.
    ]
    ; x ⇓E E(x) (E-var)
    [(? e:variable?)
      ; exp is x
      (e:env-get env exp) ;; Get the value from the environment.
    ]
    ; λx.t ⇓E {E, λx.t} (E-clos)
    [(e:lambda x t)
      (e:closure env x t) ;; Return a closure with the current unmodified env, lambda param, and body.
    ]
    ; (ef ea) ⇓E vb (E-apply)
    [(e:apply ef ea)
      ; ef ⇓ {Eb, λx.eb}
      (match (e:eval env ef)
        [(e:closure Eb x eb)
          ; ea ⇓E va
          (define va (e:eval env ea))
          ; eb ⇓Eb[x ⟼ va] vb
          (define vb (e:eval (e:env-put Eb x va) eb)) ;; Add variable x with value va into env of lambda, then evaluate lambda body (eb) with new env.
          ; return vb
          vb
        ]
      )
    ]
  )
)

;; Exercise 4 (Manually graded)
#|
One situation where λS would be a better alternative to λE would be in a teaching environment such as this class. 
The idea of directly substituting variables with their values at each step helps clarify concepts like the scope of variables within function calls. 
This can be ideal in a teaching environment because no other data structures like a map or lookup table are needed. 
However, this implementation has the problem of having a runtime of O(n). 
This is because every expression in a function’s body needs to be evaluated for substitution.
Source: https://cogumbreiro.github.io/teaching/cs450/f23/lecture14.html#24

One situation where λE would be a better alternative to λS would be when making an optimized interpreter. 
In this situation, performance would be important in order for longer sections of code to be executed efficiently. 
This would require sacrificing some memory to create a lookup table which would eliminate the need for 
going through each line of code and substituting variables at each step.
Source: https://cogumbreiro.github.io/teaching/cs450/f23/lecture14.html#30
Source: https://cogumbreiro.github.io/teaching/cs450/f23/lecture14.html#26
|#

;; Exercise 5 (Manually graded)
#|
One benefit of using formal specification when implementing software is the lack of ambiguity. 
Once you define the meaning of each symbol, there should never be any confusion about what a formal statement using those symbols means. 
Another benefit is being able to condense and visualize information. 
Using a formal notation helps to visualize concepts such as the syntax for a language like Racket. 
It also provides an abstraction to concepts like syntax by replacing things like keywords with symbols.
Source: https://cogumbreiro.github.io/teaching/cs450/f23/lecture13.html#5
|#
