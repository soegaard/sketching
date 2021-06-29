#lang sketching
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/LinearGradient/LinearGradient.pde

; Simple Linear Gradient 
; 
; The lerp-color() function is useful for interpolating between two colors.

; The default color mode is rgb:
(define b1 (color 255))           ; white
(define b2 (color   0))           ; black
(define c1 (color 204 102   0))   ; orange
(define c2 (color   0 102 153))   ; blueish

(define (setup)
  (size 640 360)
  ; hsb = hue, saturation, brightness
  (color-mode 'hsb width height 100)
  (no-loop))

(define (draw)
  ; background
  (set-gradient 0 0            (/ width 2) height  b1 b2 'x-axis) ; white->black
  (set-gradient (/ width 2) 0  (/ width 2) height  b2 b1 'x-axis) ; black->white
  ; foreground
  (set-gradient 50  90  540 80  c1 c2 'y-axis)  ; orange -> blue
  (set-gradient 50 190  540 80  c2 c1 'x-axis)) ; blue   -> orange

(define (set-gradient x y  w h  c1 c2  axis)
  (no-fill)
  (case axis
    [(y-axis) ; top to bottom gradient
     (for ([i (in-range y (+ y h 1))])
       (define inter (remap i y (+ y h) 0 1))
       (define c (lerp-color c1 c2 inter))
       (stroke c)
       (line x i (+ x w) i))]
    [(x-axis) ; left to right gradient
     (for ([i (in-range x (+ x w 1))])
       (define inter (remap i x (+ x w) 0 1))
       (define c (lerp-color c1 c2 inter))
       (stroke c)
       (line i y i (+ y h)))]))
