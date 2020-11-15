#lang racket

(define (make-env)
  (make-hash))
(define cur-Γ (make-parameter (make-env)))

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
  (match-let ([`(,name : ,typ) c])
    (hash-set! Γ name (Constructor typ))))
(define (run statement)
  (match statement
    [`(,v : ,t)
     (let ([v (eval v (cur-Γ))])
       (ty-equal? t
                  (Value-typ v))
       v)]
    [`(data ,typ-name ,constructors* ...)
     typ-name
     (for ([c constructors*])
       (parse-constructor (cur-Γ) c))]
    [`(data (,typ-name ,typ-dependencies) ,constructors* ...)
     typ-name
     (for ([c constructors*])
       (parse-constructor (cur-Γ) c))]
    [exp (displayln (eval exp (cur-Γ)))]))
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
             (Value `(,app ,@arg*) t)])]
         [else
          (displayln app)
          (displayln arg*)]))]
    [name
     (let ([v? (hash-ref Γ name #f)])
       (unless v? (error 'semantic "no identifier: ~a" name))
       (unless (Constructor? v?) (error 'semantic "not a constructor: ~a" name))
       (match (Constructor-typ v?)
         [`(→ ,t* ... ,t)
          v?]
         [t (Value name t)]))]))

(parameterize ([cur-Γ (make-env)])
  (run '(data Nat
              [Zero : Nat]
              [Suc : (→ Nat Nat)]))
  (run '(data (List T)
              [Nil : (List T)]
              [Cons : (→ T (List T))]))
  (run '(Cons Zero Nil)))

(module+ test
  (parameterize ([cur-Γ (make-env)])
    (run '(data Nat
                [Zero : Nat]
                [Suc : (→ Nat Nat)]))
    (run '(data Bool
                [True : Bool]
                [False : Bool]))
    (run 'Zero)
    (run '(Suc Zero))
    (run '((Suc (Suc Zero)) : Nat))
    ;;; error cases
    #;(run '(Suc True))))
