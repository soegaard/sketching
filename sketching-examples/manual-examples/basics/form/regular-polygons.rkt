#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Form/RegularPolygon/RegularPolygon.pde
; Regular Polygon
 
;  What is your favorite? Pentagon? Hexagon? Heptagon? 
;  No? What about the icosagon? The polygon function 
;  created for this example is capable of drawing any 
;  regular polygon. Try placing different numbers into the 
;  polygon function calls within draw to explore. 

(define (setup)
  (size 640 360)
  (frame-rate 60))

(define (draw)
  (background 102)
  ; (smoothing 'smoothed)
  
  (push-matrix)
  (translate (* 0.2 width) (* 0.5 height))
  (rotate (/ frame-count 200.0))
  (polygon 0 0 82 3) ; triangle
  (pop-matrix)

  (push-matrix)
  (translate (* 0.5 width) (* 0.5 height))
  (rotate (/ frame-count 50.0))
  (polygon 0 0 80 20) ; icosagon
  (pop-matrix)

  (push-matrix)
  (translate (* 0.8 width) (* 0.5 height))
  (rotate (/ frame-count -100.0))
  (polygon 0 0 70 7) ; heptagon
  (pop-matrix))


(define (polygon2 x y radius npoints)
  (define angle (/ 2π npoints))
  (begin-shape)
  (for ([a (in-range 0.0 2π angle)])
    (define sx (+ x (* radius (cos a))))
    (define sy (+ y (* radius (sin a))))
    (vertex sx sy))
  (end-shape 'close))


(define (polygon x y radius npoints)
  (circle x y radius))
