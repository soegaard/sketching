#lang sketching
; https://raw.githubusercontent.com/processing/processing-docs/master/content/examples/Basics/Typography/Words/Words.pde
; Words. 

;  The text() function is used for writing words to the screen.
;  The letters can be aligned left, center, or right with the 
;  textAlign() function. 

(define (setup)
  (size 640 360)
  (text-face "Arial")
  (text-size 24))

(define (draw)
  (background 102)
  (text-align 'right)
  (draw-type (* 0.25 width))
  (text-align 'center)
  (draw-type (* 0.50 width))
  (text-align 'left)
  (draw-type (* 0.75 width)))
  
(define (draw-type x)
  (line x   0  x 65)
  (line x 220  x height)
  (fill 0)
  (text "ichi" x 95)
  (fill 51)
  (text "ni" x 140)
  (fill 204)
  (text "san" x 165)
  (fill 255)
  (text "shi" x 210))
