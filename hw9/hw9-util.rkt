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
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants
(: k:const? (-> Any Boolean : k:const ))
(define (k:const? v)
  (or (k:number? v)
      (k:string? v)
      (k:bool? v)
      (k:undef? v)))

(struct k:number ([value : Real]) #:transparent)
(struct k:string ([value : String]) #:transparent)
(struct k:bool ([value : Boolean]) #:transparent)
(struct k:undef () #:transparent)
(define-type k:const (U k:number k:string k:bool k:undef))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                      SOURCE LANGUAGE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Simple JS

(define-type s:value k:const)
(define-type s:expression
  (U
    s:value
    s:variable
    s:assign
    s:invoke
    s:apply
    s:invoke
    s:load
    s:function
    s:new
    s:class
    s:let
  )
)

(struct s:variable (
  [name : Symbol]) #:transparent)

; Field lookup
; Concrete syntax: this.x
; Racket: (s:load (s:variable 'this) (s:variable 'x))
(struct s:load (
  [obj : s:variable]
  [field : s:variable]) #:transparent)

; Field update
; Concrete syntax: (set! this.x x)
; Racket: (s:assign (s:variable 'this) (s:variable 'x) (s:variable 'x))
(struct s:assign (
  [obj : s:variable]
  [field : s:variable]
  [arg : s:expression]) #:transparent)

; Function declaration
; Concrete syntax: (function (x y) e)
; Racket:
;  (s:function (list (s:variable 'x) (s:variable 'y)) e)
(struct s:function (
  [params : (Listof s:variable)]
  [body : s:expression]) #:transparent)

; Object creation
; Concrete syntax: (new Shape 0 1)
; (s:new (s:variable 'Shape) (k:number 0) (k:number 1))
(struct s:new (
  [constr : s:expression]
  [args : (Listof s:expression)]) #:transparent)

; Method invocation
; (p1.translate 10 20)
; (s:invoke (s:variable 'p1) (s:variable 'translate)
;           (list (k:number 10) (k:number 20)))
(struct s:invoke (
  [obj : s:variable]
  [meth : s:variable]
  [args : (Listof s:expression)]) #:transparent)

; Function call
; (does not show up in the homework assignment)
(struct s:apply (
  [func : s:expression]
  [args : (Listof s:expression)]) #:transparent)

; Class declaration
; (does not show up in the homework assignment)
(struct s:class (
  [parent : s:expression]
  [methods : (Immutable-HashTable s:variable s:function)]) #:transparent)

; Let binding
; (does not show up in the homework assignment)
(struct s:let (
  [name : s:variable]
  [body : s:expression]
  [kont : s:expression]) #:transparent)

;; Helper function. In JS it corresponds to: e1; e2
; (does not show up in the homework assignment)
(: s:seq (-> s:expression s:expression s:expression))
(define (s:seq e1 e2)
  (s:let (s:variable '_) e1 e2))

;; Heper function. Given a list of expressions e1...en does: e1; ...; en
; (does not show up in the homework assignment)
(: s:begin (-> (Listof s:expression) s:expression))
(define (s:begin es)
  (: stmts (Listof s:expression))
  (define stmts (reverse es))
  (if (empty? es)
    (k:undef)
    (foldl s:seq (first stmts) (rest stmts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                      TARGET LANGUAGE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type definitions
(define-type j:value k:const)
(define-type j:expression
  (U
    ; lambda
    j:value j:variable j:lambda j:apply
    ; objects
    j:object j:set j:get
    ; memory
    j:alloc j:deref j:assign
  )
)

;; Values

;; Lambda-calculus
(struct j:variable ([name : Symbol]) #:transparent)

; Function declaration
; Concrete syntax: (lambda (x) y)
; Racket: (j:lambda (list (j:variable 'x)) (j:variable 'y))
(struct j:lambda (
  [params : (Listof j:variable)]
  [body : j:expression]) #:transparent)

; Function call
; Concrete syntax: (f a b)
; Racket: (j:apply (j:variable 'f) (list (j:variable 'a) (j:variable 'b)))
(struct j:apply (
  [func : j:expression]
  [args : (Listof j:expression)]) #:transparent)

; Object declaration
; (do not use directly, use mk-object instead)
(struct j:object (
  [data :  (Immutable-HashTable k:string j:expression)]) #:transparent)

; Field lookup
; Concrete syntax: (get-field o "f")
; Racket: (j:get (j:variable 'o) (k:string "f"))
(struct j:get (
  [obj : j:expression]
  [field : j:expression]) #:transparent)

; Field update
; Concrete syntax: (update-field o "f" 10)
; Racket: (j:set (j:variable 'o) (k:string "f") (k:number 10))
(struct j:set (
  [obj  : j:expression]
  [field : k:string]
  [arg : j:expression]) #:transparent)

; Memory allocation
; (use mk-object instead)
(struct j:alloc ([value : j:expression]) #:transparent)

; Memory de-reference
; Concrete syntax: (deref this)
; Racket: (j:deref (j:variable 'this))
(struct j:deref ([value : j:expression]) #:transparent)

; Memory assignment
; Concrete syntax: (set! this o)
; Racket: (j:assign (j:variable 'this) (j:variable 'o))
(struct j:assign (
  [ref : j:expression]
  [value : j:expression]) #:transparent)

;; Represents a let binding.
;
; Formal syntax: let x = e1 in e2
; Concrete syntax: (let ([x e1]) e2)
; Racket: (mk-let e1 (lambda (x) e2))
;
(: mk-let (-> j:expression (-> j:variable j:expression) j:apply))
(define (mk-let e e-in)
  (let ([x (mk-var!)])
    (j:let x e (e-in x))))

; Concrete syntax: (begin e1 e2)
; Formal syntax: e1; e2
; Racket: (j:seq e1 e2)
(: j:seq (-> j:expression j:expression j:expression))
(define (j:seq e1 e2)
  (j:let (j:variable '_) e1 e2))

; Converts a variable into a string
; Useful when translating from SimpleJS into LambdaJS
; When you need to convert the following: x -> "x"
(: mk-field (-> s:variable k:string))
(define (mk-field x)
  (match x [(s:variable x) (k:string (symbol->string x))]))

; Allocates a j:object.
;
; Concrete syntax: (alloc (object ["foo" 1] ["bar" 2]))
; Racket: (mk-object (cons "foo" (k:number 1)) (cons "bar" (k:number 2)))
;
; Concrete syntax: (alloc (object))
; Racket: (mk-object)
(: mk-object (->* () #:rest (Pair String j:expression) j:alloc))
(define (mk-object . args)
  (: on-elem (-> (Pairof String j:expression) (Pairof k:string j:expression)))
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))

;; ------------------------------------

;; Helper function.
; (do not use; use mk-let instead)
(: j:let (-> j:variable j:expression j:expression j:apply))
(define (j:let x e e-in)
  (j:apply (j:lambda (list x) e-in) (list e)))


; (the following code support mk-let)
(: var-count (Parameter (Boxof Integer)))
(define var-count (make-parameter (box 0)))
(: mk-var! (->* () (Symbol) j:variable))
(define (mk-var! [prefix '@gen])
  (define ref (var-count))
  (define count (unbox ref))
  (set-box! ref (+ count 1))
  (j:variable (string->symbol (format "~a~a" prefix count))))

