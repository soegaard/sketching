#lang sketching

(define diagonal (dist 0 0 640 360))

(define (setup)
  (size 640 360))

(define (draw)
  (background 255)
  (stroke 0)
  (fill 0)
  (for* ([i (in-range 0 (+ width  1)  20)]
         [j (in-range 0 (+ height 1) 20)])
    (define size (dist mouse-x mouse-y i j))
    (define s    (* 66 (/ size diagonal)))
    (ellipse i j s s)))



