#lang sketching

(define (setup)
  (size (* 1 640) (* 1 360))
  (frame-rate 30)
  (no-stroke)
  #;(stroke "black"))

(define (draw)
  (color-mode 'hsb 360 1. 1. 100)
  (translate (/ width 2) (/ height 2))
  (for ([s (in-range 0. 1. 0.1)])
    (for ([i (in-range 0 360 11)])
      (define r (radians i))
      (fill (modulo i 360) s 1 50)  
      (circle (* s 100 (cos r)) (* s 100 (sin r)) 20))))


