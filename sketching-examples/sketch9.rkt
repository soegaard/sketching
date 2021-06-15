#lang sketching

(define (setup)
  (size 600 600))

(define (draw)
  (stroke 0)
  (stroke-weight 5)
  (line pmouse-x pmouse-y mouse-x mouse-y))
