#lang racket
(require sketching)

(define (setup)
  (size 600 400))

(define (draw)
  (if (< mouse-x 300)
      (cursor 'cross)
      (cursor 'hand)))

(setup)
(current-draw draw)
(start)
