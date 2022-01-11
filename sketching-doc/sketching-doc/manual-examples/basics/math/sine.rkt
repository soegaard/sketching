#lang sketching

#| 
Sketching example: Sine

This implementation is a direct translation from the "Sine" example from https://processing.org/examples/sine.html.
|#

(define diameter #f)
(define angle     0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (frame-rate 60)
  (:= diameter (- height 10))
  (fill 255 204 0))

(define (draw)
  (background 0)
  (let* ([dh (/ diameter 2)]
         [hh (/ height 2)]
         [wh (/ width 2)]
         [d1 (+ 10 (* (sin angle) dh) dh)]
         [d2 (+ 10 (* (sin (+ angle (/ pi 2))) dh) dh)]
         [d3 (+ 10 (* (sin (+ angle pi)) dh) dh)])
    (ellipse     0 hh d1 d1)
    (ellipse    wh hh d2 d2)
    (ellipse width hh d3 d3)
    (:= angle (+ angle 0.02))))
