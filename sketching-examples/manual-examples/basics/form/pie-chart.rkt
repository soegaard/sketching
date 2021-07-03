#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Form/PieChart/PieChart.pde
; Pie Chart  
;  
;  Uses the arc() function to generate a pie chart from the data
;  stored in an array. 

(define angles (vector 30 10 45 35 60 38 75 67))

(define (setup)
  (size 640 360)
  (no-stroke)
  (no-loop)) ; run draw only once

(define (draw)
  (background 100)
  (pie-chart 300 angles))

(define (pie-chart diameter data)
  (define last-angle 0)
  (for ([a angles] [i (in-naturals)])
    (define gray (remap i 0 angles.length 0 255))
    (fill gray)
    (arc (* 1/2 width) (* 1/2 height) diameter diameter last-angle (+ last-angle (radians a)))
    (+= last-angle (radians a))))
