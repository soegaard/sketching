#lang sketching

#| 
Sketching example: Sine Wave

This implementation is a close translation from the "Sine Wave" example from https://processing.org/examples/sinewave.html by Daniel Shiffman.
|#

(require (only-in racket/list
                  range))

(define x-spacing 16)
(define w         #f)
(define theta      0)
(define amplitude 75)
(define period   500)
(define dx        #f)
(define y-values '())

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (:= w (+ width 16))
  (:= dx (* (/ (* pi 2) period) x-spacing))
  (:= y-values (for/list ([_ (range (/ w x-spacing))])
                 #f)))

(define (draw)
  (background 0)
  (calc-wave)
  (render-wave))

(define (calc-wave)
  (:= theta (+ theta 0.02))
  (let ([x theta])
    (:= y-values (for/list ([v y-values])
                   (begin0
                       (* (sin x) amplitude)
                     (:= x (+ x dx)))))))

(define (render-wave)
  (no-stroke)
  (fill 255)
  (for ([v (range (length y-values))])
    (ellipse (* v x-spacing)
             (+ (/ height 2)
                (list-ref y-values v))
             16
             16)))
