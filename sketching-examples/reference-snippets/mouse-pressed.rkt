#lang sketching

(define (draw)
  (cond
    [mouse-pressed  (fill 0)]
    [else           (fill 255)])
  (rect 25 25 50 50))
