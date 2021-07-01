#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Transform/Scale/Scale.pde
; Scale 
;  Original Processing example by Denis Grutze.

(define a 0.0)
(define s 0.0)

(define (setup)
  (size 640 360)
  (rect-mode 'center)
  (no-stroke)
  (frame-rate 30))

(define (draw)
  (background 102)

  (:= a (+ a 0.04))
  (:= s (* 2 (cos a)))
  
  (translate (* 1/2 width) (* 1/2 height))
  (scale s)
  (fill 51)
  (rect 0 0 50 50)
  
  (translate 75 0)
  (fill 255)
  (scale s)
  (rect 0 0 50 50))
