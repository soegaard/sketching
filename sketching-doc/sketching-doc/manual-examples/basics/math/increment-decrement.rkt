#lang sketching

#| 
Sketching example: Increment Decrement

This implementation is a direct translation from the "Increment Decrement" example:
https://processing.org/examples/incrementdecrement.html

The expression (--  a) decrements a and returns the new value.
|#

(define a         #f)
(define b         #f)
(define direction #t)

(define (setup)
  (size 640 360)
  (:= a -1)
  (:= b width)
  (color-mode 'hsb width)
  (frame-rate 60))

(define (draw)
  (++ a)
  (when (> a width)
    (:= a 0)
    (:= direction (not direction)))
  (if direction
      (stroke a)
      (stroke (- width a)))
  (line a 0 a (/ (height) 2))
  (-- b)
  (when (< b 0)
    (:= b width))
  (if direction
      (stroke (- width b))
      (stroke b))
  (line b (add1 (/ height 2)) b height))
