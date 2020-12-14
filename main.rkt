#lang racket

(require "core.rkt"
         "substmap.rkt"
         "lang.rkt")
(require nanopass/base)

(define (make-env)
  (make-hash))
(define cur-Γ (make-parameter (make-env)))

(define (bind id val)
  (hash-set! (cur-Γ) id val))
(define (lookup id)
  (hash-ref (cur-Γ) id #f))

(define (<-type val)
  (match val
    [(Value _ ty) ty]))

(define (parse-constructor name typ [dependencies #f])
  (match typ
    [`(→ ,t* ...)
     #:when dependencies
     (bind name (Constructor `(→ ,@(map (λ (t) (replace-occur #:in t #:occur dependencies)) t*))))]
    [ty #:when dependencies
        (bind name (Constructor (replace-occur #:in ty #:occur dependencies)))]
    [else (bind name (Constructor typ))]))
(define (run statement)
  (define-parser parse-Incr Incr)
  (run/nano (parse-Incr statement)))
(define-pass run/nano : Incr (stmt) -> * ()
  (Stmt : Stmt (stmt) -> * ()
        [(data (,x [,x0 ,: ,t0] ...)
               (,x* ,:1 ,t*) ...)
         (define deps (make-hash))
         (for ([d t0])
           (match d
             [`(,d ,dt)
              (hash-set! deps d (FreeVar d dt))]
             [d (hash-set! deps d (FreeVar d 'U))]))
         (for ([c-name x*]
               [c-typ t*])
           (parse-constructor c-name c-typ deps))]
        [(data ,x (,x* ,: ,t*) ...)
         (for ([c-name x*]
               [c-typ t*])
           (parse-constructor c-name c-typ))]
        [(define (,x [,x* ,: ,t*] ...) ,:1 ,t
           ,e)
         (let ([s (make-subst)]
               [e (eval exp)])
           (for ([p-name x*]
                 [p-typ t*])
             'todo)
           (unify t (<-type e) #:subst s)
           (error 'todo)
           (bind x 'todo))]
        [(define ,x ,: ,t ,e)
         (let ([s (make-subst)]
               [e (eval e)])
           (unify t (<-type e) #:subst s)
           (bind x e))]
        [,e (displayln (eval e))]))
(define (eval e)
  (nanopass-case
   (Incr Expr) e
   [(,e ,e* ...)
    (let ([appliable (eval e)]
          [arg* (map (λ (arg) (eval arg)) e*)])
      (match appliable
        [(Constructor typ)
         (nanopass-case
          (Incr Typ) typ
          [(→ ,t* ... ,t)
           (let ([subst (make-subst)])
             ; TODO: currying fix
             (for ([expect t*]
                   [arg arg*])
               (unify expect (Value-typ arg) #:subst subst))
             (Value `(,e ,@arg*)
                    (replace-occur #:in t #:occur (subst-resolve subst))))])]
        [(Value _ _) appliable]
        [else (error 'semantic "not appliable: ~a" e)]))]
   [,x
    (let* ([name x]
           [v? (lookup name)])
      (unless v? (error 'semantic "no identifier: ~a" name))
      (cond
        [(Constructor? v?)
         (nanopass-case
          (Incr Typ) (Constructor-typ v?)
          [(→ ,t* ... ,t)
           v?]
          [else (Value name (Constructor-typ v?))])]
        [(Value? v?) v?]
        [else (error 'unknown "unknown: ~a => ~a" name v?)]))]))

(define pre-defined-Γ (make-env))
(parameterize ([cur-Γ pre-defined-Γ])
  (run '(data Nat
              [Zero : Nat]
              [Suc : (→ Nat Nat)]))
  (run '(data Bool
              [True : Bool]
              [False : Bool])))

#;(parameterize ([cur-Γ pre-defined-Γ])
    (run '(define one : Nat
            (Suc Zero)))
    (run 'one)

    (run '(define (id [x : T]) : T x)))

(module+ test
  (parameterize ([cur-Γ pre-defined-Γ])
    (run 'Zero)
    (run '(Suc (Suc Zero)))
    ;; error case: semantic: type mismatched, expected: Nat, but got: Bool
    #;(run '(Suc True)))

  (parameterize ([cur-Γ pre-defined-Γ])
    (run '(data (List [T : U])
                [Nil : (List T)]
                [Cons : (→ T (List T) (List T))]))
    (run '(Cons Zero Nil))
    ;; error case: semantic: type mismatched, expected: `Bool`, but got: `Nat`
    #;(run '(Cons Zero (Cons True Nil))))

  (parameterize ([cur-Γ pre-defined-Γ])
    (run '(data (Vec [T : U] [N : Nat])
                [Nil : (Vec T Zero)]
                [Cons : (→ T (Vec T N) (Vec T (Suc N)))]))
    (run '(Cons Zero Nil))
    (run '(Cons (Suc Zero) (Cons Zero Nil)))
    ;; error case: semantic: type mismatched, expected: `Nat`, but got: `Bool`
    #;(run '(Cons True (Cons Zero Nil))))

  )
