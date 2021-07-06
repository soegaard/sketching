#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/Keyboard/Keyboard.pde
; Keyboard.

; Click on the image to give it focus and press the letter keys 
; to create forms in time and space. Each key has a unique identifying 
; number. These numbers can be used to position shapes in space. 

(define rect-width 0)

(define (setup)
  (size 640 360)
  (no-stroke)
  (background 0)
  (:= rect-width (/ width 4)))

(define (draw)
  (void))

(define (on-key-pressed)
  (define c key)
  (cond
    [(and (char? c) (char-alphabetic? c))
     (define key-index (- (int (char-downcase c)) (int #\a)))
     (fill (modulo (millis) 255)) ; color depends on time
     (define x (remap key-index 0 25 0 (- width rect-width)))
     (rect x 0 rect-width height)]
    [else
     ; blank the scren on non-letters
     (background 0)]))
