#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/Mouse1D/Mouse1D.pde

; Move the mouse left and right to shift the balance.
; The mouse-x variable is used to control both the size and color of the rectangles.

(define (setup)
  (size 640 360)
  (no-stroke)
  (color-mode 'rgb height height height)
  (rect-mode 'center))

(define (draw)
  (background 0)

  (define r1 (remap mouse-x 0 width 0 height))
  (define r2 (- height r1))

  (fill (int r1))
  (rect (/ (+ width r1) 2) (/ height 2) r1 r1)

  (fill (int r2))
  (rect (/ (- width r1) 2) (/ height 2) r2 r2))
