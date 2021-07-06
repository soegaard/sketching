#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/StoringInput/StoringInput.pde
; Storing Input.

; Move the mouse across the screen to change the position
; of the circles. The positions of the mouse are recorded
; into an array and played back every frame. Between each
; frame, the newest value are added to the end of each array
; and the oldest value is deleted. 

(define n 60)
(define mx (make-vector n 0.0))
(define my (make-vector n 0.0))

 
(define (setup)
  (size 640 360)
  (frame-rate 60))

(define (draw)
  (background 51)
  (no-stroke)
  (fill 255 153)

  ; The arrays are used as circular buffers.
  ; This way we don't need to move anything.
  (define which (modulo frame-count n))
  (:= mx which mouse-x)
  (:= my which mouse-y)

  (for ([i n])
    ; which+1 is the index of the oldest entry
    (define index (modulo (+ which 1 i) n))
    (ellipse (mx.ref index) (my.ref index) i i)))
