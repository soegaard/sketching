#lang sketching
(require racket/math)
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/RadialGradient/RadialGradient.pde

; Radial Gradient.

; Draws a series of concentric circles to create a gradient from one color to another.

(define dim 1)

(define (setup)
  (size 640 360)
  (:= dim (/ width 2))
  (background 0)
  (color-mode 'hsb 360 100 100)
  (no-stroke)
  (ellipse-mode 'radius)
  (background 0)
  (frame-rate 1))

(define (draw)
  (for ([x (in-range 0 (+ width 1) dim)])
    (draw-gradient x (/ height 2))))

(define (draw-gradient x y)
  (define radius (quotient dim 2))
  (define h (exact-floor (random 0 360)))
  (for ([r (in-range radius 0 -1)])
    (fill h 90 90)
    (ellipse x y r r)
    (:= h (modulo (+ h 1) 360))))
