#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Transform/Arm/Arm.pde
; Arm.

;  The angle of each segment is controlled with the mouseX and
;  mouseY position. The transformations applied to the first segment
;  are also applied to the second segment because they are inside
;  the same pushMatrix() and popMatrix() group.

(define x      0.0)
(define y      0.0)
(define angle1 0.0)
(define angle2 0.0)

(define seg-length 100)

(define (setup)
  (size 640 360)
  (stroke-weight 30)
  (stroke 255 160)

  (:= x (* 0.3 width))
  (:= y (* 0.5 height)))

(define (draw)
  (background 0)

  (define angle1 (* -1 π (- (/ mouse-x width)  0.5)))
  (define angle2 (* -1 π (- (/ mouse-y height) 0.5)))

  (push-matrix)
  (segment x y angle1)
  (segment seg-length 0 angle2)
  (pop-matrix))

(define (segment x y a)
  (translate x y)
  (rotate a)
  (line 0 0 seg-length 0))
