#lang racket/base

(require racket/match)
(require "substmap.rkt")

(provide (struct-out Constructor))
(struct Constructor (typ) #:transparent)

(provide (struct-out Function))
(struct Function (param-name* param-typ* typ expr) #:transparent)

(provide (struct-out Value))
(struct Value (v typ)
  #:methods gen:custom-write
  [(define (write-proc value port mode)
     (fprintf port "~a" (Value-v value)))]
  #:transparent)

(provide replace-occur)
(define (replace-occur #:in in #:occur occurs)
  (match in
    [`(,e* ...)
     (map (λ (e) (replace-occur #:in e #:occur occurs)) e*)]
    [v (let ([new-v (hash-ref occurs v #f)])
         (if new-v new-v v))]))

(define (full-expand exp occurs)
  (match exp
    [`(,e* ...)
     (map (λ (e) (full-expand e occurs)) e*)]
    [v (let ([new-v (hash-ref occurs v #f)])
         (if new-v (full-expand new-v occurs) v))]))
(provide unify)
(define (unify t1 t2 #:subst [subst (make-subst)])
  (match* {t1 t2}
    [{(? FreeVar?) _} (subst-set! subst t1 t2)]
    [{_ (? FreeVar?)} (unify t2 t1 #:subst subst)]
    [{`(,t1* ...) `(,t2* ...)}
     (map (λ (t1 t2) (unify t1 t2 #:subst subst))
          t1* t2*)]
    [{_ _}
     (unless (equal? t1 t2)
       (error 'semantic "type mismatched, expected: `~a`, but got: `~a`" t1 t2))])
  (full-expand t1 (subst-resolve subst)))

(module+ test
  (require rackunit)

  (check-equal? (unify (FreeVar 'c 'U) '(-> A B)) '(-> A B))
  (let ([s (make-subst)])
    (unify (FreeVar 'a 'U) 'A #:subst s)
    (unify (FreeVar 'b 'U) 'B #:subst s)
    (check-equal?
     (unify (FreeVar 'c 'U) `(-> ,(FreeVar 'a 'U) ,(FreeVar 'b 'U)) #:subst s)
     '(-> A B))))