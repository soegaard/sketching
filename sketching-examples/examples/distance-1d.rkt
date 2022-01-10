#lang sketching

#| 
Sketching example: Distance 1D

This implementation is a direct translation from the "Distance 1D" example from https://processing.org/examples/distance1d.html.
|#

(define x-pos-1 #f)
(define x-pos-2 #f)
(define x-pos-3 #f)
(define x-pos-4 #f)
(define thin     8)
(define thick   36)

(define (setup)
  (size 640 360)
  (no-stroke)
  (frame-rate 60)
  (set! x-pos-1 (/ width 2))
  (set! x-pos-2 (/ width 2))
  (set! x-pos-3 (/ width 2))
  (set! x-pos-4 (/ width 2)))

(define (draw)
  (background 0)
  (let ([mx (- (* mouse-x 0.4) (/ width 5))]
        [hh (/ height 2)])
    (fill 102)
    (rect x-pos-2 0 thick hh)
    (fill 204)
    (rect x-pos-1 0 thin  hh)
    (fill 102)
    (rect x-pos-4 hh thick hh)
    (fill 204)
    (rect x-pos-3 hh thin hh)

    (set! x-pos-1 (+ x-pos-1 (/ mx 16)))
    (set! x-pos-2 (+ x-pos-2 (/ mx 64)))
    (set! x-pos-3 (- x-pos-3 (/ mx 16)))
    (set! x-pos-4 (- x-pos-4 (/ mx 64)))

    (when (< x-pos-1 (- thin))
      (set! x-pos-1 width))
    (when (> x-pos-1 width)
      (set! x-pos-1 (- thin)))
    (when (< x-pos-2 (- thick))
      (set! x-pos-2 width))
    (when (> x-pos-2 width)
      (set! x-pos-2 (- thick)))
    (when (< x-pos-3 (- thin))
      (set! x-pos-3 width))
    (when (> x-pos-3 width)
      (set! x-pos-3 (- thin)))
    (when (< x-pos-4 (- thick))
      (set! x-pos-4 width))
    (when (> x-pos-4 width)
      (set! x-pos-4 (- thick)))))
