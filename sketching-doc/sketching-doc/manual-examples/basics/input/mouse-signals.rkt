#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/MouseSignals/MouseSignals.pde
; Mouse Signals.

; Move and click the mouse to generate signals. 
; The top row is the signal from "mouseX", 
; the middle row is the signal from "mouseY",
; and the bottom row is the signal from "mousePressed". 

(define w 640) ; width
(define h 360) ; height

(define xs (make-vector w))
(define ys (make-vector w))
(define bs (make-vector w))

(define (setup)  
  (size 640 360)
  (frame-rate 30)
  (stroke-weight 16))

(define (draw)
  (background 102)
  (stroke-weight 2)

  ; shift all values left
  (for ([i (in-range 1 w)])
    (:= xs (- i 1) (xs.ref i))
    (:= ys (- i 1) (ys.ref i))
    (:= bs (- i 1) (bs.ref i)))

  ; enter new values at the right
  (:= xs (- w 1) mouse-x)
  (:= ys (- w 1) mouse-y)

  (if mouse-pressed
      (:= bs (- w 1) 0)
      (:= bs (- w 1) (/ height 3)))
  
  (fill 255)
  (no-stroke)
  (rect 0 (/ height 3) width (+ (/ height 3) 1))

  (for ([i (in-range 1 width)])
    ; draw x-values
    (stroke 255)
    (point i (remap (xs.ref i) 0 width 0 (- (/ height 3) 1)))
    ; draw y-values
    (stroke 0)
    (point i (+ (/ height 3) (/ (ys.ref i) 3)))
    ; draw mouse presses
    (stroke 255)
    (line i (+ (* 2/3 height) (bs.ref i))
          i (+ (* 2/3 height) (bs.ref (- i 1))))))
