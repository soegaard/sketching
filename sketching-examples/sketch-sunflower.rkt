#lang sketching
; http://algorithmicbotany.org/papers/abop/abop-ch4.pdf

(define c 5)

(define (setup)
  (size 600 600))

(define (draw)
  (define n frame-count)
  (define φ (* n (radians 137.5)))
  (define r (* c (sqrt n)))

  ; (stroke 0)
  (stroke-weight 5)
  (translate (/ width 2) (/ height 2))
  (point (* r (cos φ)) (* r (sin φ))))
