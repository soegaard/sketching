#lang sketching
;; Drag (click and hold) your mouse across the 
;; image to change the color of the rectangle.

(define col 0)

(define (draw)
  (fill col)            
  (rect 25 25 50 50))

(define (on-mouse-dragged)
  (:= col (+ col 5))
  (when (> col 255)
    (:= col 0)))
