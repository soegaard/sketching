#lang sketching

#| 
Sketching example: Polar to Cartesian

This implementation is a close translation from the "Polar to Cartesian" example from https://processing.org/examples/polartocartesian.html by Daniel Shiffman.
|#

(define r          #f)
(define theta      0)
(define theta-vel  0)
(define theta-acc  0.0001)

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (ellipse-mode 'center)
  (no-stroke)
  (fill 200)
  (:= r (* height 0.45)))

(define (draw)
  (background 0)
  (translate (/ width 2) (/ height 2))
  (let ([x (* r (cos theta))]
        [y (* r (sin theta))])
    (ellipse x y 32 32)
    (:= theta-vel (+ theta-vel theta-acc))
    (:= theta (+ theta theta-vel))))

