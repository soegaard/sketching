#lang sketching
; Original example by Rusty Robinson
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/Hue/Hue.pde

; Hue is the color reflected from or transmitted through an object 
; and is typically referred to as the name of the color (red, blue, yellow, etc.) 
; Move the cursor vertically over each bar to alter its hue. 


(define bar-width 20)
(define last-bar  -1)

(define (setup)
  (size 640 360)  
  (color-mode 'hsb height height height)
  (background 0)
  (no-stroke))

(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (unless (= which-bar last-bar)
    (define bar-x (* which-bar bar-width))
    (fill mouse-y height height)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))
