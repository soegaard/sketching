#lang racket
(require sketching)

(define max-distance 1)

(define (setup)
  (frame-rate 60)
  (size 640 360)
  (set! max-distance (dist 0 0 640 360)))

(define (draw)
  (background 255)
  (stroke 0)
  (fill 0)
  (for* ([i (in-range 0 (+ width  1)  20)]
         [j (in-range 0 (+ height 1) 20)])
    (define size (dist mouse-x mouse-y i j))
    (define s    (* 66 (/ size max-distance)))    
    (ellipse i j s s)))

(setup)
(current-draw draw)
(start)




