#lang racket

(require "core.rkt"
         "substmap.rkt")

(struct env (parent map)
  #:transparent)
(define (make-env [parent #f])
  (env parent (make-hash)))
(define cur-Γ (make-parameter (make-env)))

(define (bind id val)
  (hash-set! (env-map (cur-Γ)) id val))
(define (lookup id)
  (let ([env (cur-Γ)])
    (if env
        (hash-ref (env-map env) id
                  (parameterize ([cur-Γ (env-parent env)])
                    (lookup id)))
        #f)))

(define (<-type val)
  (match val
    [(Value _ ty) ty]))

(define (parse-constructor c [dependencies #f])
  (match-let ([`(,name : ,typ) c])
    (match typ
      [`(→ ,t* ...)
       #:when dependencies
       (bind name (Constructor `(→ ,@(map (λ (t) (replace-occur #:in t #:occur dependencies)) t*))))]
      [ty #:when dependencies
          (bind name (Constructor (replace-occur #:in ty #:occur dependencies)))]
      [else (bind name (Constructor typ))])))
(define (run statement)
  (match statement
    [`(,v : ,t)
     (let ([v (eval v)])
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
       (parse-constructor c deps))]
    [`(data ,typ-name ,constructors* ...)
     typ-name
     (for ([c constructors*])
       (parse-constructor c))]
    [`(define (,name [,param-name* : ,param-typ*] ...) : ,typ
        ,exp)
     (bind name (Function param-name* param-typ* typ exp))]
    [`(define (,name param* ...) : ,typ ,exp)
     (error 'todo)]
    [`(define ,name : ,typ ,exp)
     (let ([s (make-subst)]
           [e (eval exp)])
       (unify typ (<-type e) #:subst s)
       (bind name e))]
    [exp (displayln (eval exp))]))
(define (eval exp)
  (match exp
    [`(match ,e
        [,pat* => ,pat-e*] ...)
     (match-let ([(Value val typ) (eval e)])
       (define (pattern-equal? pat val)
         (match pat
           [`(,first ,rest ...)
            (and (pattern-equal? first (car val))
                 (pattern-equal? rest (cdr val)))]
           ['_ #t]
           [n (equal? n val)]))
       (define result
         (ormap (λ (pat pat-e)
                  (if (pattern-equal? pat val)
                      (eval pat-e)
                      #f))
                pat* pat-e*))
       (unless result
         (error 'semantic "match fail ~a" exp))
       result)]
    [`(,app ,arg* ...)
     (let ([appliable (eval app)]
           [arg* (map (λ (a) (eval a)) arg*)])
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
         [(Function param-name* param-typ* typ expr)
          (let ([env (make-env (cur-Γ))]
                [subst (make-subst)])
            (parameterize ([cur-Γ env])
              ; TODO: currying fix
              (for ([param param-name*]
                    [expect param-typ*]
                    [arg arg*])
                (unify expect (Value-typ arg) #:subst subst)
                (bind param arg))
              (Value (eval expr)
                     (replace-occur #:in typ #:occur (subst-resolve subst)))))]
         [(Value _ _) appliable]
         [else (error 'semantic "not appliable: ~a" app)]))]
    [name
     (let ([v? (lookup name)])
       (unless v? (error 'semantic "no identifier: ~a" name))
       (cond
         [(Constructor? v?)
          (match (Constructor-typ v?)
            [`(→ ,t* ... ,t)
             v?]
            [t (Value name t)])]
         [(Function? v?) v?]
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

(parameterize ([cur-Γ pre-defined-Γ])
  (run '(define (is-zero? [n : Nat]) : Bool
          (match n
            [Zero => True]
            [(Suc _) => False])))

  (run '(is-zero? Zero))
  (run '(is-zero? (Suc Zero))))

#;(module+ test
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
      #;(run '(Cons True (Cons Zero Nil))))

    (parameterize ([cur-Γ pre-defined-Γ])
      (run '(define one : Nat
              (Suc Zero)))
      (run 'one)

      (run '(define (add2 [n : Nat]) : Nat
              (Suc (Suc n))))

      (run '(add2 Zero))
      (run '(add2 (Suc Zero)))))
