#lang sketching
; Original Processing example by hbarragan.
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Objects/Objects/

;;;
;;; Objects
;;;

;; Move the cursor across the image to change the speed and positions
;; of the geometry. The class MRect defines a group of lines.

(class MRect Object
  (init-field w xpos h ypos d t)
       ; w     single bar width
       ; xpos  rect xposition
       ; h     rect height
       ; ypos  rect yposition
       ; d     single bar distance
       ; t      number of bars

  ;; Constructor
  ; Nothing to do - values are used as is
  (super-new)
  
  ;; Methods
  (define/public (move pos-x pos-y damping)
    (define dif (- ypos pos-y))
    (when (> (abs dif) 1)
      (-= ypos (/ dif damping)))
    (:= dif (- xpos pos-x))
    (when (> (abs dif) 1)
      (-= xpos (/ dif damping))))

  (define/public (display)
    (for ([i t])
      (rect (+ xpos (* i (+ d w))) ypos w (* height w)))))

;;;
;;; Example 
;;;

(define r1 (make-object MRect 1 134.0 0.532  (* 0.1 height) 10.0 60))
(define r2 (make-object MRect 2  44.0 0.166  (* 0.3 height)  5.0 50))
(define r3 (make-object MRect 2  58.0 0.332  (* 0.4 height) 10.0 35))
(define r4 (make-object MRect 1 120.0 0.0498 (* 0.9 height) 15.0 60))

(define (setup)
  (size 640 360)
  (fill 255 204)
  (frame-rate 60)
  (no-stroke))

(define (draw)
  (background 0)
  (r1.display)
  (r2.display)
  (r3.display)
  (r4.display)

  (r1.move (- mouse-x (/ width 2))                           (+ mouse-y (* height 0.1))   30)
  (r2.move (modulo (int (+ mouse-x (* width 0.05))) width)   (+ mouse-y (* height 0.025)) 20)
  (r3.move (/ mouse-x 4)                                     (- mouse-y (* height 0.025)) 40)
  (r4.move (- mouse-x (/ width 2))                           (- height mouse-y)           50))


