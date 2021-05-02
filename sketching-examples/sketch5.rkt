#lang racket
(require sketching)

(define (setup)
  (frame-rate 30)
  (size 600 400))

(define (draw)
  (stroke 255)
  (cond
    [key-pressed
     (write key) (newline)
     (case key
       [(#\r) (fill "red")]
       [(#\g) (fill "green")]
       [(#\b) (fill "blue")]
       [else  (fill "white")])
     (ellipse mouse-x mouse-y 40 40)]
    [key-released
     (fill "white")]))

(setup)
(current-draw draw)
(start)
