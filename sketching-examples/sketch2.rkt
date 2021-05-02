#lang racket
(require sketching)

(define (setup)
  (size 600 400)
  (frame-rate 30))

(define x   0)
(define y 200)

(define v 1)
(define a 0.1)

(define (next)
  (set! x (+ x 1))
  (set! y (+ y v))
  (set! v (+ v a))
  (when (>= y height)
    (set! y height)
    (set! v -5)))
  (when (<= y 0)
    (set! y 0)
    (set! v 0.1))

(define (draw)
  (next)
  (stroke "black")
  (fill "yellow")
  (no-fill)
  ; (no-stroke)
  (circle x y 20))

(current-draw draw)
(setup)
(start)


