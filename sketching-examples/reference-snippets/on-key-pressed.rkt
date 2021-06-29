#lang sketching

(define col 0)

(define (draw)
  (fill col)            
  (rect 25 25 50 50))

(define (on-key-pressed)
  (:= col (- 255 col)))

