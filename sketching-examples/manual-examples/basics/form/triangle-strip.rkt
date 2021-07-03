#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Form/TriangleStrip/TriangleStrip.pde
; Triangle Strip 
;  by Ira Greenberg. 
;  
;  Generate a closed ring using the vertex() function and 
;  beginShape(TRIANGLE_STRIP) mode. The outsideRadius and insideRadius 
;  variables control ring's radii respectively.

(define x 0.0)
(define y 0.0)

(define outside-radius 150)
(define inside-radius 100)

(define (setup)
  (size 640 360)
  (background 204)
  (:= x (* 1/2 width))
  (:= y (* 1/2 height)))
  
(define (draw)
  (background 204)

  (define angle      0)
  (define num-points (int (remap mouse-x 0 width 6 60)))
  (define angle-step (/ 180.0 num-points))

  (begin-shape 'triangle-strip)
  (for ([i (+ num-points 1)])
    (define px (+ x (* outside-radius (cos (radians angle)))))
    (define py (+ y (* outside-radius (sin (radians angle)))))
    (+= angle angle-step)
    (vertex px py)
    (define qx (+ x (*  inside-radius (cos (radians angle)))))
    (define qy (+ y (*  inside-radius (sin (radians angle)))))
    (+= angle angle-step)
    (vertex qx qy))
  (end-shape))
