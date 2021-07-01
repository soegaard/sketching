#lang sketching
; https://github.com/processing/processing-docs/tree/master/content/examples/Basics/Arrays/ArrayObjects
; Vector of objects.

; Demonstrates how to create a vector of objects.

(class Blob Object
  (field [x-offset 0]
         [y-offset 0]
         [x        0]
         [y        0]
         [unit     1]
         [x-dir    1]
         [y-dir    1]
         [speed    1])

  ; "Constructor"  
  (init x-offset-temp y-offset-temp x-temp y-temp speed-temp temp-unit)
  
  (super-new)    ; super class initialization
  (:= x-offset x-offset-temp)
  (:= y-offset y-offset-temp)
  (:= x        x-temp)
  (:= y        y-temp)
  (:= speed    speed-temp)
  (:= unit     temp-unit)
  
  
  ; Methods
  (define/public (update)
    (:= x (+ x (* speed x-dir)))
    (unless (<= 0 x unit)
      (*= x-dir -1)
      (:= x (+ x x-dir))
      (:= y (+ y y-dir)))

    (unless (<= 0 y unit)
      (*= y-dir -1)
      (:= y (+ y y-dir))))

  (define/public (draw)
    (fill 255)
    (ellipse (+ x x-offset) (+ y y-offset) 6 6)))

;;; ---------------

(define unit  40)
(define count 0)
(define blobs (vector))

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (no-stroke)
  (:= blobs  (for*/vector  ; for* is a nested loop
                 ([x (/ width  unit)]
                  [y (/ height unit)])
               (new Blob
                    [x-offset-temp (* x   unit)]
                    [y-offset-temp (* y   unit)]
                    [x-temp        (* 1/2 unit)]
                    [y-temp        (* 1/2 unit)]
                    [speed-temp    (random 0.05 0.8)]
                    [temp-unit     unit]))))

(define (draw)
  (background 0)
  (for ([blob blobs])
    (blob.update)
    (blob.draw)))
