#lang racket
(require sketching)

(define (setup)
  (size 600 600))

(define (draw)
  (stroke 0)
  (stroke-weight 5)
  (unless (and (= pmouse-x pmouse-y) (= mouse-x pmouse-y))
    (line pmouse-x pmouse-y mouse-x pmouse-y)))

(setup)
(current-draw draw)
(start)
