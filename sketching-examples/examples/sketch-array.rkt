#lang sketching
; Move the mouse up and down.
; The y-coordinates are recorded in the vector ys.
; A scrolling graph displays the results.

(define ys (vector 1))

(define (setup)
  (size 640 360)
  (:= ys (make-vector width 0.)))

(define (draw)
  (background 204)

  ; shift all ements to the right
  (for ([i (in-range (- width 1) 0 -1)])
    (:= ys i ys[(- i 1)] ))

  ; record new y
  (:= ys 0 mouse-y)

  ; draw graph
  (for ([i (in-range 1 width)])
    (line i ys[i] (- i 1) ys[(- i 1)] )))

(displayln "Move the mouse.")
