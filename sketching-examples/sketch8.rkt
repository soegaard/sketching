#lang racket
(require sketching)

(define (setup)
  (size 200 200))

(define (draw)
  (background (* 3 64))
  (stroke 0)

  (fill 255)
  (rect 0 0 50 50) ; white rectangle

  (push-matrix)
  (translate 30 20)
  (fill 0)
  (rect 0 0 50 50) ; black rectangle
  (pop-matrix)

  (fill 100)
  (rect 15 10 50 50)) ; gray 
  
(setup)
(current-draw draw)
(start)

