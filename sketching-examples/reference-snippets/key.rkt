#lang sketching

(define (draw)
  (cond
    [(and key-pressed (or (equal? key #\b) (equal? key #\B)))
     (fill 0)]
    [else
     (fill 255)])
  (rect 25 25 50 50))
