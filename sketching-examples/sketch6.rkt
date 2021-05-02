#lang racket
(require sketching)

(define ball (load-image "amiga-ball.png"))

(define (setup)
  (size 600 400))

(define (draw)
  (background 255)
  (image ball mouse-x mouse-y))


(setup)
(current-draw draw)
(start)
