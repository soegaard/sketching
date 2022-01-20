#lang sketching

#| 
Sketching example: Graphing 2D Equation

This implementation is a close translation from the "Graphing 2D Equation" example from https://processing.org/examples/graphing2dequation.html.

Currently, pixel operations are very slow.
|#

(define (setup)
  (size 600 380)
  (color-mode 'rgb)
  (frame-rate 60))

#|
The safe-atan2 function is here to prevent the atan2 function to be called with x = y = 0.
Mathematically, this function is not defined at the point (0, 0), but to make our life easier we just define it as (atan2 0 0) -> 0.
|#
(define (safe-atan2 x y)
  (if (= x y 0)
      0
      (atan2 x y)))

(define (draw)
  (load-pixels)
  (let* ([n (/ (* mouse-x 10) width)]
         [w                       16]
         [h                       16]
         [dx             (/ w width)]
         [dy            (/ h height)]
         [x              (/ (- w) 2)])
    (for ([i width])
      (let ([y (- (/ h 2))])
        (for ([j height])
          (let* ([r     (sqrt (+ (* x x) (* y y)))]
                 [theta (safe-atan2 y x)]
                 [val   (sin (+ (* n (cos r)) (* 5 theta)))])
            (set-pixel i j (color (* (+ val 1.) (/ 255. 2.))))
            (:= y (+ y dy))))
        (:= x (+ x dx)))))
  (update-pixels))

