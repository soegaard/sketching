#lang sketching

#| 
Sketching example: Distance 2D

This implementation is a direct translation from the "Distance 2D" example from https://processing.org/examples/distance2d.html.
|#

(require (only-in racket/list
                  inclusive-range))

(define max-distance #f)

(define (setup)
  (size 800 600)
  (no-stroke)
  (frame-rate 60)
  (:= max-distance (dist 0 0 width height)))

(define (draw)
  (background 0)
  (for* ([i (inclusive-range 0 width  20)]
         [j (inclusive-range 0 height 20)])
    (let* ([size  (dist mouse-x mouse-y i j)]
           [size  (* (/ size max-distance) 66)])
      (ellipse i j size size))))
