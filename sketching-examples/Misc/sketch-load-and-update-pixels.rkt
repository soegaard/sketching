#lang sketching

(define (setup)
  (frame-rate 1)
  (size 640 360)
  (color-mode 'rgb width height 100))

(define load? #t)

(define (draw)
  (when load?
    (load-pixels)
    (set! load? #f))
  (time
   (begin     
     (for* ([x width] [y height])
       (define c (color x y 50))
       ; (void)
       (set-pixel x y c))
     (update-pixels))))
