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
(struct FreeVar (name)
  #:methods gen:custom-write
  [(define (write-proc var port mode)
     (fprintf port "free{~a}" (FreeVar-name var)))]
  #:transparent)

(define (ty-equal? #:subst [subst (make-hash)] expect actual)
  (match expect
    [(FreeVar name)
     (when (hash-ref subst name #f)
       (error 'semantic "rebound"))
     (hash-set! subst name actual)]
    [else (unless (equal? expect actual)
            (error 'semantic "type mismatched, expected: ~a, but got: ~a" expect actual))])
  subst)
(define (replace-occur #:in in #:occur occurs)
  (match in
    [`(,e* ...)
     (map (λ (e) (replace-occur #:in e #:occur occurs)) e*)]
    [v (let ([new-v (hash-ref occurs v #f)])
         (if new-v new-v v))]))
(define (parse-constructor Γ c [dependencies #f])
  (match-let ([`(,name : ,typ) c])
    (match typ
      [`(→ ,t* ... ,t)
       #:when dependencies
       (hash-set! Γ name
                  (Constructor `(→ ,@(map (λ (t) (replace-occur #:in t #:occur dependencies)) t*) ,t)))]
      [ty #:when dependencies
          (hash-set! Γ name (Constructor (replace-occur #:in ty #:occur dependencies)))]
      [else (hash-set! Γ name (Constructor typ))])))
(define (run statement)
  (match statement
    [`(,v : ,t)
     (let ([v (eval v (cur-Γ))])
       (ty-equal? t (Value-typ v))
       v)]
    [`(data (,typ-name ,typ-dependency* ...) ,constructors* ...)
     typ-name
     (define deps (make-hash))
     (for ([d typ-dependency*])
       (hash-set! deps d (FreeVar d)))
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
             (let ([subst (make-hash)])
               ; TODO: currying fix
               (for ([expect t*]
                     [actual (map (λ (a) (Value-typ a)) arg*)])
                 (ty-equal? expect actual #:subst subst))
               (Value `(,app ,@arg*)
                      (replace-occur #:in t #:occur subst)))])]
         [else (error 'semantic "not appliable: ~a" app)]))]
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
  (run '(data Bool
              [True : Bool]
              [False : Bool]))
  (run '(data (List T)
              [Nil : (List T)]
              [Cons : (→ T (List T) (List T))]))
  (run '(Cons Zero Nil))
  (run '(Cons Zero (Cons True Nil))))

#;(module+ test
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
