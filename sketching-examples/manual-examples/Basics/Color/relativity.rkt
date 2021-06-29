#lang sketching
;   https://raw.githubusercontent.com/processing/processing-docs/master/content/examples/Basics/Color/Relativity/Relativity.pde


; Relativity.

; Each color is perceived in relation to other colors. The top and bottom 
; bars each contain the same component colors, but a different display order 
; causes individual colors to appear differently.


(define a (color 165 167  20))
(define b (color  77  86  59))
(define c (color  42 106 105))
(define d (color 165  89  20))
(define e (color 146 150 127))

(define (setup)
  (size 640 360)
  (no-loop) ; only call draw once
  (color-mode 'hsb height height height))


(define (draw)
  (draw-band a b c d e 0 (/ width 128))
  (draw-band c a d b e (/ height 2) (/ width 128)))


(define (draw-band v w x y z y-pos bar-width)
  (define colors (list v w x y z))
  (define num 5) ; number of colors
  (for ([i (in-range 0 width (* bar-width num))])
    (for ([j (length colors)] [c colors])
      (fill c)
      (rect (+ i (* j bar-width)) y-pos bar-width (/ height 2)))))
