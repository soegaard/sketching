#lang sketching
; 
; Star
 

(define (setup)
  (size 640 360)
  (frame-rate 60))

(define (draw)
  (background 102)
  (smoothing 'smoothed)
  
  (push-matrix)
  (translate (* 0.2 width) (* 0.5 height))
  (rotate (/ frame-count 200.0))
  (star 0 0 5 70 3)
  (pop-matrix)

  (push-matrix)
  (translate (* 0.5 width) (* 0.5 height))
  (rotate (/ frame-count 400.0))
  (star 0 0 80 100 40)
  (pop-matrix)

  (push-matrix)
  (translate (* 0.8 width) (* 0.5 height))
  (rotate (/ frame-count -100.0))
  (star 0 0 30 70 5) 
  (pop-matrix))


(define (star x y radius1 radius2 npoints)
  (define angle (/ 2π npoints))
  (begin-shape)
  (for ([a (in-range 0.0 2π angle)])
    (define sx (+ x (* radius2 (cos a))))
    (define sy (+ y (* radius2 (sin a))))
    (define tx (+ x (* radius1 (cos (+ a (* 1/2 angle))))))
    (define ty (+ y (* radius1 (sin (+ a (* 1/2 angle))))))
    (vertex sx sy)
    (vertex tx ty))
  (end-shape 'close))
