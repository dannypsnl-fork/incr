#lang racket

(define init-Γ (make-hash))

(struct Constructor (typ) #:transparent)
(struct Value (v typ)
  #:methods gen:custom-write
  [(define (write-proc value port mode)
     (fprintf port "~a" (Value-v value)))]
  #:transparent)

(define (ty-equal? expect actual)
  (unless (equal? expect actual)
    (error 'semantic "type mismatched, expected: ~a, but got: ~a" expect actual)))
(define (parse-constructor Γ c)
  (match c
    [`(,name : ,typ)
     (hash-set! Γ name (Constructor typ))]))
(define (run Γ statement)
  (match statement
    [`(,v : ,t)
     (let ([v (eval v Γ)])
       (ty-equal? t
                  (Value-typ v))
       v)]
    [`(data ,typ-name ,constructors* ...)
     typ-name
     (for ([c constructors*])
       (parse-constructor Γ c))]
    [exp (eval exp Γ)]))
(define (eval exp Γ)
  (match exp
    [`(,app ,arg* ...)
     (let ([appliable (eval app Γ)]
           [arg* (map (λ (a) (eval a Γ)) arg*)])
       (match appliable
         [(Constructor typ)
          (match typ
            [`(→ ,t* ... ,t)
             (for ([expect t*]
                   [actual (map (λ (a) (Value-typ a)) arg*)])
               (ty-equal? expect actual))
             (Value `(,app ,@arg*) t)])]))]
    [name
     (let ([v? (hash-ref Γ name #f)])
       (unless v? (error 'semantic "no identifier: ~a" name))
       (unless (Constructor? v?) (error 'semantic "not a constructor: ~a" name))
       (match (Constructor-typ v?)
         [`(→ ,t* ... ,t)
          v?]
         [t (Value name t)]))]))

(run init-Γ '(data Nat
                   [Zero : Nat]
                   [Suc : (→ Nat Nat)]))
(run init-Γ '(data Bool
                   [True : Bool]
                   [False : Bool]))
(run init-Γ 'Zero)
(run init-Γ '(Suc Zero))
(run init-Γ '((Suc (Suc Zero)) : Nat))
;;; error cases
;@(run init-Γ '(Suc True))
