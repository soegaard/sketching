#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Arrays/Array2D/Array2D.pde
; Two-dimensional vector

;  Demonstrates the syntax for creating a two-dimensional (2D) array.
;  Values in a 2D array are accessed through two index values.  
;  2D arrays are useful for storing images. In this example, each dot 
;  is colored in relation to its distance from the center of the image. 

(define w   640)
(define h   360)
(define w/2 (* 1/2 w))
(define h/2 (* 1/2 h))

(define spacer 10)

(define max-distance (dist w/2 h/2 w h))
(define distances
  (for/vector ([x w])
    (for/vector ([y h])
      ; distance from screen center to (x,y)
      (remap (dist x y w/2 h/2)
             0 max-distance 0 255))))

(define (setup)
  (size w h)
  (stroke-weight 6)
  (no-loop)) ; run draw only once

(define (draw)
  (background 0)
  ; This embedded loop skips over values in the arrays based on
  ; the spacer variable, so there are more values in the array
  ; than are drawn here. Change the value of the spacer variable
  ; to change the density of the points
  (for ([y (in-range 0 height spacer)])
    (for ([x (in-range 0 width spacer)])
      (define ys (distances.ref x))
      (stroke (ys.ref y))
      (point (+ x (/ spacer 2)) (+ y (/ spacer 2))))))
