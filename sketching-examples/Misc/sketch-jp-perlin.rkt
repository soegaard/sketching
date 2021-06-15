#lang sketching
(require
 noise
 racket/math)

(define (setup)
  (size 640 360))

(define (draw)
  (no-stroke)
  (rect-mode 'corner)
  (color-mode 'hsb 360 100 100 100)
  (define w 20)
  (define h 20)
  (define t (/ frame-count 20))
  (for ([x (in-range 0 width w)])
    (for ([y (in-range 0 height h)])
      (define x0 (/ x width))
      (define y0 (/ y height))
      (define noise (perlin x0 y0 t))
      (define z (sin (* noise 1)))
      (fill (int (* (+ 0.5 noise) 360))
            100 100)
      (rect x y w h))))
