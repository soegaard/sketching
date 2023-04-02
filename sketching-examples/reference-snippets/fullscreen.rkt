#lang sketching

(define x 0)

(define (setup)
  (fullscreen)
  (background 0)
  (no-stroke)
  (fill 102))

(define (draw)
  (background 255)
  (rect mouse-x (* 0.2 height) 1 (* 0.6 height))
  (:= x (+ x 2)))
