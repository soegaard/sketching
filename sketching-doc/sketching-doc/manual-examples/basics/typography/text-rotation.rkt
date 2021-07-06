#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Typography/TextRotation/TextRotation.pde
; Text Rotation. 

; Draws letters to the screen and rotates them at different angles.


(define angle 0.0) ; the rotation angle

(define (setup)
  (size 640 360)
  (frame-rate 10)
  (text-size 18)
  (background 0))

(define (draw)
  (background 0)
  ; (smoothing 'unsmoothed)
  ; (text-smoothing 'unsmoothed)

  (stroke-weight 1)
  (stroke 153)

  (push-matrix)
  (define angle1 (radians 45))
  (translate 100 180)
  (rotate angle1)
  (text "45 DEGREES" 0 0)
  (line 0 0 150 0)
  (pop-matrix)

  (push-matrix)
  (define angle2 (radians 270))
  (translate 200 180)
  (rotate angle2)
  (text "270 DEGREES" 0 0)
  (line 0 0 150 0)
  (pop-matrix)

  (push-matrix)
  (translate 440 180)
  (rotate (radians angle))
  (text (~a (modulo (int angle) 360) " DEGREES") 0 0)
  (line 0 0 150 0)
  (pop-matrix)
  
  (+= angle 0.25)
      
  (stroke 255 0 0)
  (stroke-weight 4)
  (point 100 180)
  (point 200 180)
  (point 440 180))
