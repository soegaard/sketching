#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Transform/Translate/Translate.pde
; Translate.

;  The translate function moves the position of (0,0).
;  If you draw an object (in this example a rectangle) relative
;  to (0,0), you can use translate to move the position of the object.

(define x    0.0)
(define y    0.0)

(define dim 80.0)

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (rect-mode 'center))

(define (draw)
  (background 102)
  (no-stroke)
  
  (:= x (+ x 0.8))
  (when (> x (+ width dim))
    (:= x (- dim)))
  
  (translate x (- (/ height 2) (/ dim 2)))
  (fill 255)
  (rect 0 0 dim dim)
  
  ; Transforms accumulate. Notice how this rect moves 
  ; twice as fast as the other, but it has the same 
  ; argument for the x-axis value
  (translate x dim)
  (fill 0)
  (rect 0 0 dim dim))
