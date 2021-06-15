#lang sketching
(require racket/match racket/list)

(define (displaced-midpoint p q max-displacement)
  (match-define (list x1 y1) p)
  (match-define (list x2 y2) q)
  (define r (random (- max-displacement) max-displacement))
  (list      (/ (+ x1 x2) 2.)
        (+ r (/ (+ y1 y2) 2.))))

(define (add-midpoints ps max-displacement)
  (define mid-points
    (for/list ([p ps] [q (rest ps)])
      (displaced-midpoint p q max-displacement)))
  (interleave ps mid-points))

(define (interleave as bs)
  (if (empty? as)
      bs
      (cons (first as)
            (interleave bs (rest as)))))

(define (displaced-midpoints points max-displacement iterations [iteration 1])
  (define (loop ps n md)
    (if (= n iterations)
        ps
        (loop (add-midpoints ps md)
              (+ n 1)
              (/ md 1.8))))
  (loop points 0 max-displacement))


(define (setup)
  ; (frame-rate 2)
  (size 640 360)
  (stroke "black")
  (background "white")
  (rect-mode 'corner)
  #;(no-loop))

(define (draw)
  (color-mode 'rgb 255 255 255 100)
  (fill 255 255 255 1)
  (rect 0 0 width height)
  ; (fill frame-count 50 100)
  (color-mode 'hsb 360 100 100)
  (define points (list->vector
                  (displaced-midpoints '((0 0) (640 0))
                                       20
                                       5)))
  ; (displayln points)
  (define ymid (/ height 2))
  (for ([p points] [q (in-vector points 1 (vector-length points))])
    (match-define (list x1 y1) p)
    (match-define (list x2 y2) q)
    (fill frame-count 50 100)
    ; (stroke "red")
    (no-stroke)
    (quad x1 height   x1 (+ y1 ymid)   x2 (+ ymid y2)  x2 height)))


              
