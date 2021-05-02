#lang racket/base
(require racket/draw
         racket/class
         "parameters.rkt")


(provide rotate
         scale
         translate
         push-matrix
         pop-matrix
         )

(define (translate dx dy)
  (send dc translate dx dy))

(define (rotate angle) ; radians
  (send dc rotate angle))

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

    
  
