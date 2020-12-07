#lang racket/base

(require racket/match
         racket/hash)

(struct subst (bound-map free-map) #:transparent)
(provide make-subst)
(define (make-subst)
  (subst (make-hash) (make-hash)))

(provide subst-set!)
(define (subst-set! subst key value)
  (match* {key value}
    [{(? FreeVar?) (? FreeVar?)}
     (let ([cur-bound? (hash-ref (subst-free-map subst) key #f)])
       (if cur-bound?
           (hash-set! (subst-free-map subst) key (cons value cur-bound?))
           (hash-set! (subst-free-map subst) key (list value))))]
    [{(? FreeVar?) _}
     (define cur-bound? (hash-ref (subst-bound-map subst) key #f))
     (when (and cur-bound? (not (equal? cur-bound? value)))
       (error 'semantic "type mismatched, expected: `~a`, but got: `~a`" value cur-bound?))
     (hash-set! (subst-bound-map subst) key value)]))

(provide subst-resolve)
(define (subst-resolve subst)
  (define resolved-map (make-hash))
  (hash-for-each (subst-free-map subst)
                 (λ (k v*)
                   (let ([bound? (hash-ref (subst-bound-map subst) k #f)])
                     (unless bound?
                       (error 'semantic "~a unsolvable" v*))
                     (for ([v v*])
                       (hash-set! resolved-map v bound?)))))
  (hash-union! resolved-map (subst-bound-map subst)
               #:combine/key (λ (k a b) a))
  resolved-map)

(provide (struct-out FreeVar))
(struct FreeVar (name typ)
  #:methods gen:custom-write
  [(define (write-proc var port mode)
     (fprintf port "(@~a : ~a)" (FreeVar-name var) (FreeVar-typ var)))]
  #:transparent)
