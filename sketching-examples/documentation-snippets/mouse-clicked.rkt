#lang sketching
; Click within the image
; to change the color of the rectangle.

(define col 0)

(define (draw)
  (fill 0)            
  (rect 25 25 50 50))

(define (mouse-clicked)
  (:= col (- 255 col)))


