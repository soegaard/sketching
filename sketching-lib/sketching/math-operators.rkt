#lang racket/base
(provide += -= *= /=
         ; :=
         ++ --)

(require (for-syntax racket/base syntax/parse)
         "dot-method.rkt")

(define-syntax (+= stx)
  (syntax-parse stx
    [(_+= x:id e)
     (syntax/loc stx
       (:= x (+ x e)))]

    [(_+= v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i (+ (vector-ref v i) e2))))))]))

(define-syntax (-= stx)
  (syntax-parse stx
    [(_+= x:id e)
     (syntax/loc stx
       (:= x (- x e)))]

    [(_+= v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i (- (vector-ref v i) e2))))))]))

(define-syntax (*= stx)
  (syntax-parse stx
    [(_+= x:id e)
     (syntax/loc stx
       (:= x (* x e)))]

    [(_+= v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i (* (vector-ref v i) e2))))))]))

(define-syntax (/= stx)
  (syntax-parse stx
    [(_+= x:id e)
     (syntax/loc stx
       (:= x (/ x e)))]

    [(_+= v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i (/ (vector-ref v i) e2))))))]))


#;(define-syntax (:= stx)
  (syntax-parse stx
    [(_ x:id e)
     (syntax/loc stx
       (set! x e))]

    [(_ v:id e1 e2)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e1)
           (when (and (integer? i) (not (negative? i)))
             (vector-set! v i e2)))))]))

(define-syntax (++ stx)
  (syntax-parse stx
    [(_++ x:id)
     (syntax/loc stx
       (begin (:= x (+ x 1)) x))]

    [(_++ v:id e)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e)
           (when (and (integer? i) (not (negative? i)))
             (let ([t (+ (vector-ref v i) 1)]
                   (vector-set! v i t)
                   t))))))]))

(define-syntax (-- stx)
  (syntax-parse stx
    [(_ x:id)
     (syntax/loc stx
       (begin (:= x (- x 1)) x))]

    [(_ v:id e)
     (syntax/loc stx
       (let ()
         (when (vector? v)
           (define i e)
           (when (and (integer? i) (not (negative? i)))
             (let ([t (- (vector-ref v i) 1)]
                   (vector-set! v i t)
                   t))))))]))

                    
