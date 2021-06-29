#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/Easing/Easing.pde
; Easing. 

; Move the mouse across the screen and the symbol will follow.  
; Between drawing each frame of the animation, the program
; calculates the difference between the position of the 
; symbol and the cursor. If the distance is larger than
; 1 pixel, the symbol moves part of the distance (0.05) from its
; current position toward the cursor. 

(define x 0.)
(define y 0.)
(define easing 0.05)

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (no-stroke))

(define (draw)
  (background 51)
  
  (define target-x mouse-x)
  (define dx (- target-x x))
  (+= x (* dx easing))

  (define target-y mouse-y)
  (define dy (- target-y y))
  (+= y (* dy easing))
  
  (ellipse x y 66 66))

