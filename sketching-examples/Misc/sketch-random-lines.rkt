#lang sketching

(define (setup)
  (size 800 600)
  (stroke-cap 'butt)
  (frame-rate 10))

(define (draw)
  (background 255)
  (stroke 0 0 0 20)
  (for ([i (in-range 10000)])
    (define x0 (random (* -2 width)  (* 2 width)))
    (define y0 (random (* -2 height) (* 2 height)))
    (define x1 (random (* -2 width)  (* 2 width)))
    (define y1 (random (* -2 height) (* 2 height)))
    (line x0 y0 x1 y1)))
