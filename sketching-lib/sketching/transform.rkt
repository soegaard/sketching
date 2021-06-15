#lang racket/base
(require racket/draw
         racket/class
         "parameters.rkt")


(provide rotate
         scale
         translate
         push-matrix
         pop-matrix
         reset-matrix
         )

(define (translate dx dy)
  (send dc translate dx dy))

(define (rotate angle) ; radians
  ; The - is due to ... is the Racket rotate going clockwise?
  (send dc rotate (- angle)))

(define scale
  (case-lambda
    [(s)   (send dc scale s s)]
    [(x y) (send dc scale x y)]
    [else (error 'scale "incorrect arguments")]))


(define the-matrix-stack '())
(define (push! m)
  (set! the-matrix-stack (cons m the-matrix-stack)))
(define (pop!)
  (define m (car the-matrix-stack))
  (set! the-matrix-stack (cdr the-matrix-stack))
  m)

(define (push-matrix)
  (push! (send dc get-transformation)))

(define (pop-matrix)
  (define m (pop!))
  (send dc set-transformation m))

(define (reset-matrix)
  ; (define initial (send dc get-initial-matrix))
  (define initial (vector 1. 0. 0. 1. 0. 0.))
  (send dc set-transformation (vector initial 0. 0. 1. 1. 0.)))
    
  
