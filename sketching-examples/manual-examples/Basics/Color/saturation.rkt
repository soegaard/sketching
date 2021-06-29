#lang sketching
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/Saturation/Saturation.pde

; Saturation is the strength or purity of the color and represents the 
; amount of gray in proportion to the hue. A "saturated" color is pure 
; and an "unsaturated" color has a large percentage of gray. 
; Move the cursor vertically over each bar to alter its saturation.

(define bar-width 20)
(define last-bar  -1)

(define (setup)
  (size 640 360)
  (no-stroke)
  (background 0)
  (color-mode 'hsb width height 100))

(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (unless (= which-bar last-bar)
    (define bar-x (* which-bar bar-width))
    (fill bar-x mouse-y 66)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))
