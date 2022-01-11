#lang sketching

#| 
Sketching example: Sine Cosine

This implementation is a direct translation from the "Sine Cosine" example from https://processing.org/examples/sinecosine.html.
|#

(define x1      #f)
(define x2      #f)
(define y1      #f)
(define y2      #f)
(define angle-1  0)
(define angle-2  0)
(define scalar  70)

(define (setup)
  (size 640 360)
  (no-stroke)
  (rect-mode 'center)
  (frame-rate 60))

(define (draw)
  (background 0)
  (let ([ang-1 (radians angle-1)]
        [ang-2 (radians angle-2)]
        [wh (/ width  2)]
        [hh (/ height 2)])
    (:= x1 (+ wh (* scalar (cos ang-1))))
    (:= x2 (+ wh (* scalar (cos ang-2))))
    
    (:= y1 (+ hh (* scalar (sin ang-1))))
    (:= y2 (+ hh (* scalar (sin ang-2))))

    (fill 255)
    (rect wh hh 140 140)

    (fill 0 102 153)
    (ellipse x1 (- hh 120) scalar scalar)
    (ellipse x2 (+ hh 120) scalar scalar)

    (fill 255 204 0)
    (ellipse (- wh 120) y1 scalar scalar)
    (ellipse (+ wh 120) y2 scalar scalar)

    (:= angle-1 (+ angle-1 2))
    (:= angle-2 (+ angle-2 3))))
