#lang racket

(require "core.rkt"
         "substmap.rkt")

(define (make-env)
  (make-hash))
(define cur-Γ (make-parameter (make-env)))

(define (parse-constructor Γ c [dependencies #f])
  (match-let ([`(,name : ,typ) c])
    (match typ
      [`(→ ,t* ...)
       #:when dependencies
       (hash-set! Γ name
                  (Constructor `(→ ,@(map (λ (t) (replace-occur #:in t #:occur dependencies)) t*))))]
      [ty #:when dependencies
          (hash-set! Γ name (Constructor (replace-occur #:in ty #:occur dependencies)))]
      [else (hash-set! Γ name (Constructor typ))])))
(define (run statement)
  (match statement
    [`(,v : ,t)
     (let ([v (eval v (cur-Γ))])
       (unify t (Value-typ v))
       v)]
    [`(data (,typ-name ,typ-dependency* ...) ,constructors* ...)
     typ-name
     (define deps (make-hash))
     (for ([d typ-dependency*])
       (match d
         [`(,d ,dt)
          (hash-set! deps d (FreeVar d dt))]
         [d (hash-set! deps d (FreeVar d 'U))]))
     (for ([c constructors*])
       (parse-constructor (cur-Γ) c deps))]
    [`(data ,typ-name ,constructors* ...)
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
             (let ([subst (make-subst)])
               ; TODO: currying fix
               (for ([expect t*]
                     [arg arg*])
                 (unify expect (Value-typ arg) #:subst subst))
               (Value `(,app ,@arg*)
                      (replace-occur #:in t #:occur (subst-resolve subst))))])]
         [else (error 'semantic "not appliable: ~a" app)]))]
    [name
     (let ([v? (hash-ref Γ name #f)])
       (unless v? (error 'semantic "no identifier: ~a" name))
       (unless (Constructor? v?) (error 'semantic "not a constructor: ~a" name))
       (match (Constructor-typ v?)
         [`(→ ,t* ... ,t)
          v?]
         [t (Value name t)]))]))

(module+ test
  (define pre-defined-Γ (make-env))
  (parameterize ([cur-Γ pre-defined-Γ])
    (run '(data Nat
                [Zero : Nat]
                [Suc : (→ Nat Nat)]))
    (run '(data Bool
                [True : Bool]
                [False : Bool])))

  (parameterize ([cur-Γ pre-defined-Γ])
    (run 'Zero)
    (run '(Suc Zero))
    (run '((Suc (Suc Zero)) : Nat))
    ;; error case: semantic: type mismatched, expected: Nat, but got: Bool
    #;(run '(Suc True)))

  (parameterize ([cur-Γ pre-defined-Γ])
    (run '(data (List T)
                [Nil : (List T)]
                [Cons : (→ T (List T) (List T))]))
    (run '(Cons Zero Nil))
    ;; error case: semantic: type mismatched, expected: `Bool`, but got: `Nat`
    #;(run '(Cons Zero (Cons True Nil))))

  (parameterize ([cur-Γ pre-defined-Γ])
    (run '(data (Vec T [N Nat])
                [Nil : (Vec T Zero)]
                [Cons : (→ T (Vec T N) (Vec T (Suc N)))]))
    (run '(Cons Zero Nil))
    (run '(Cons (Suc Zero) (Cons Zero Nil)))
    ;; error case: semantic: type mismatched, expected: `Nat`, but got: `Bool`
    #;(run '(Cons True (Cons Zero Nil)))))
