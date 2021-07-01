#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Arrays/Array/Array.pde
;  Vector. 

;  Vectors are zero based, which means that the first 
;  element in the array has index 1, the second element 1, and so on. 
;  In this example, a vector named "coswave" is created and filled
;  with cosine values. This data is displayed three  separate ways on the screen.  

(define w 640)
(define h 360)

(define y1 #f)
(define y2 #f)

(define coswave (for/vector ([i w]) (abs (cos (remap i 0 w 0 pi)))))

(define (setup)
  (size 640 360)
  (background 255)
  (no-loop))

(define (draw)
  (:= y1 0)
  (:= y2 (* 1/3 height))
  (for ([i w] [c coswave])
    (stroke (* c 255))
    (line i y1 i y2))

  (:= y1 y2)
  (:= y2 (+ y1 y1))
  (for ([i w] [c coswave])
    (stroke (* 1/2 (* c 255)))
    (line i y1 i y2))

  (:= y1 y2)
  (:= y2 height)
  (for ([i w] [c coswave])
    (stroke (- 255 (* c 255)))
    (line i y1 i y2)))
