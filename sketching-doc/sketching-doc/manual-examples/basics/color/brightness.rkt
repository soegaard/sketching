#lang sketching
; Original example by Rusty Robinson
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/Brightness/Brightness.pde

(define bar-width 20)
(define last-bar  -1)

(define (setup)
  (size 640 360)
  (color-mode 'hsb width 100 height)
  (background 0)
  (no-stroke))

(define (draw)
  (define which-bar (quotient mouse-x bar-width))
  (unless (= which-bar last-bar)
    (define bar-x (* which-bar bar-width))
    (fill bar-x 100 mouse-y)
    (rect bar-x 0 bar-width height)
    (set! last-bar which-bar)))

