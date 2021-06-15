#lang sketching

(define (draw)
  (cond
    [(and mouse-pressed (eq? mouse-button 'left))  (fill 0)]
    [(and mouse-pressed (eq? mouse-button 'right)) (fill 255)]
    [else                                          (fill 127)])
  (rect 25 25 50 50))
