#lang sketching

(define (draw)
  (background 255)
  (cond
    [focused (ellipse 25 25 50 50)]
    [else    (line   0 0 100 100)
             (line 100 0   0 100)]))

