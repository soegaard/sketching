#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/MousePress/MousePress.pde
; Move the mouse to position the shape. 
; Press the mouse button to invert the color.

(define (setup)
  (size 640 360)
  (no-smooth)
  (fill 126)
  (background 102))

(define (draw-cross x y)
  (line (- x 66)    y       (+ x 66)    y)
  (line    x     (- y 66)      x     (+ y 66)))

(define (draw)
  (if mouse-pressed (stroke 255) (stroke 0))
  (draw-cross mouse-x mouse-y))
