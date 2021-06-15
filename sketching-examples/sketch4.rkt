#lang sketching

(define (setup)
  (frame-rate 30)
  (size 480 120))

(define (draw)
  (stroke 255)
  (if mouse-pressed
      (begin (stroke 255) (fill 255))
      (begin (stroke 0)   (fill 0)))
  (ellipse mouse-x mouse-y 40 40))




