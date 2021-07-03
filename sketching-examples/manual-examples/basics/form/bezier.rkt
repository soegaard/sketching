#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Form/Bezier/Bezier.pde
; Bezier. 
;  
;  The first two parameters for the bezier() function specify the 
;  first point in the curve and the last two parameters specify 
;  the last point. The middle parameters set the control points
;  that define the shape of the curve. 

(define (setup)
  (size 640 360)
  (stroke 255)
  (no-fill))

(define (draw)
  (background 0)
  (for ([i (in-range 0. 200 20)])
    (bezier (- mouse-x (* 1/2 i)) (+ i 40)
            410 20
            440 300
            (- 240 (* 1/16 i)) (+ 200 (* 1/8 i)))))
