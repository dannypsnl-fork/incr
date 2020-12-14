#lang racket/base

(require nanopass/base)

(provide (all-defined-out))

(define (:? x)
  (equal? ': x))
(define (variable? x)
  (symbol? x))

(define-language Incr
  (entry Stmt)
  (terminals
   (variable (x))
   (: (:)))
  (Stmt (stmt)
        (x : t)
        (data (x [x0 : t0] ...)
              (x1 :1 t1) ...)
        (data x (x0 : t0) ...)
        (define (x [x* : t*] ...) :1
          t e)
        (define x : t e)
        e)
  (Expr (e)
        x
        (e e* ...))
  (Typ (t)
       (→ t0 t* ... t1)
       (x t* ...)
       x))

(module+ test
  (define-parser parse-Incr Incr)

  (parse-Incr
   '(data Nat
           [z : Nat]
           [s : (→ Nat Nat)]))
  (parse-Incr
   '(data (List [T : U])
          [nil : (List T)]
          [:: : (→ T (List T) (List T))]))
  (parse-Incr
   '(data (Vec [T : U] [N : Nat])
          [nil : (Vec T z)]
          [:: : (→ T (Vec T N) (Vec T (s N)))]))
  (parse-Incr
   '(define one : Nat z))
  (parse-Incr
   '(define (add2 [n : Nat]) : Nat
      (s (s n))))
  (parse-Incr
   '(s z)))
