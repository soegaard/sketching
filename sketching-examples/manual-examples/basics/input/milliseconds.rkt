#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/Milliseconds/Milliseconds.pde
; Milliseconds. 

; A millisecond is 1/1000 of a second. 
; Processing keeps track of the number of milliseconds a program has run.
; By modifying this number with the modulo, different patterns in time are created.

(define scale 1)

(define (setup)
  (size 640 360)
  (frame-rate 30)
  (:= scale (/ width 20)))

(define (draw)
  (no-stroke)
  (for ([i (in-range scale)])
    (define n (* (+ i 1) scale 10))
    (color-mode 'rgb n)
    (fill (modulo (millis) n))
    (rect (* i scale) 0 scale height)))
