#lang sketching
(require racket/format)

(define (setup)
  (size 640 360)
  (background "black")
  (color-mode 'hsb 360 255 255 100))

(define (draw)
  (define time (~a (hour) ":" (minute) ":" (second)))

  (background "black")

  (fill "white")
  (text time (+ 15 4) (- 100 4))
  
  (fill frame-count 128 255)
  (text-size 128)
  (text time 15 100))
