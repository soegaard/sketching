#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Form/PointsLines/PointsLines.pde
; Points and Lines. 
;  
;  Points and lines can be used to draw basic geometry.
;  Change the value of the variable 'd' to scale the form.
;  The four variables set the positions based on the value of 'd'. 

(define d  70)
(define p1       d)
(define p2 (+ p1 d))
(define p3 (+ p2 d))
(define p4 (+ p3 d))

(define (setup)
  (size 640 360)
  (no-smooth)
  (background 0)
  (translate 140 40))

(define (draw)
  ; gray box
  (stroke 153)
  (line p3 p3 p2 p3)
  (line p2 p3 p2 p2)
  (line p2 p2 p3 p2)
  (line p3 p2 p3 p3)
  
  ; white points
  (stroke 255)
  (stroke-weight 4)
  (point p1 p1)
  (point p1 p3)
  (point p2 p4)
  (point p3 p1)
  (point p4 p2)
  (point p4 p4))
