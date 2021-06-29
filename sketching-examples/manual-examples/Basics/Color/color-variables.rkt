#lang sketching
; Original:
;   https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Color/ColorVariables/ColorVariables.pde

;; Color Variables (Homage to Albers).
;;
;; This example creates variables for colors that may be referred to 
;; in the program by a name, rather than a number. 


(define (setup)
  (size 640 360))

(define inside  (color 204 102 0))
(define middle  (color 204 153 0))
(define outside (color 153  51 0))


(define (draw)
  ; setup
  (no-stroke)
  (background 51 0 0)
  ; draw
  (push-matrix)
  (translate 80 80)
  (fill outside)
  (rect 0 0 200 200)
  (fill middle)
  (rect 40 60 120 120)
  (fill inside)
  (rect 60  90 80 80)
  (pop-matrix)

  (push-matrix)
  (translate 360 80)
  (fill inside)
  (rect 0 0 200 200)
  (fill outside)
  (rect 40 60 120 120)
  (fill middle)
  (rect 60 90 80 80)
  (pop-matrix))
