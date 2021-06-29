#lang sketching

(define (setup)
  (set-frame-rate! 30)
  (rect-mode 'center))

(define (draw)
  (background 0)
  (fill 255)
  (text (~a frame-count) 50 50))
