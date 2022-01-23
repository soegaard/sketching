#lang sketching

#| 
Sketching example: Remap

This implementation is a direct translation from the "Map" example from 
https://processing.org/examples/map.html.
|#

(define (setup)
  (size 640 360)
  (no-stroke)
  (frame-rate 60))

(define (draw)
  (background 0)
  (let ([c (max (remap mouse-x 0 width  0 175) 0)]
        [d (remap mouse-x 0 width 40 300)])
    (fill 255 c 0)
    (ellipse (/ width 2) (/ height 2) d d)))
