#lang sketching

#| 
Sketching example: Arctangent

This implementation is a close translation from the "Arctangent" example from https://processing.org/examples/arctangent.html.
|#

(require (only-in racket/match
                  match-let))

(struct eye (x y s a))

(define (update-eye e mx my)
  (match-let ([(eye x y _ _) e])
    (struct-copy eye e
                 [a (atan2 (- my y) (- mx x))])))

(define (display-eye e)
  (push-matrix)
  (match-let ([(eye x y s a) e])
    (translate x y)
    (fill 255)
    (ellipse 0 0 s s)
    (rotate a)
    (fill 153 204 0)
    (ellipse (/ s 4) 0 (/ s 2) (/ s 2)))
  (pop-matrix))

(define eye-1 (eye 250  16 120 0))
(define eye-2 (eye 164 185  80 0))
(define eye-3 (eye 420 230 220 0))

(define (setup)
  (size 640 360)
  (frame-rate 60)
  (no-stroke))

(define (draw)
  (background 102)
  (:= eye-1 (update-eye eye-1 mouse-x mouse-y))
  (display-eye eye-1)
  (:= eye-2 (update-eye eye-2 mouse-x mouse-y))
  (display-eye eye-2)
  (:= eye-3 (update-eye eye-3 mouse-x mouse-y))
  (display-eye eye-3))
