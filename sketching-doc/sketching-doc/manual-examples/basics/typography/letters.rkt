#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Typography/Letters/Letters.pde
; Letters.

;  Draws letters to the screen. 

(define (setup)
  (size 640 360)
  (background 0)

  (text-face "Tahoma")
  (text-size 24)
  (text-align 'center 'center))

(define (draw)
  ; set the left and top marings
  (define margin 10)
  (translate (* 4 margin) (* 4 margin))

  (define gap     46)
  (define counter 35)

  (for ([y (in-range 0 (- height gap) gap)])
    (for ([x (in-range 0 (- width gap) gap)])
      ; Current letter
      (define letter (char counter))
      ; Color wovel and consonants differently
      (case (char-downcase letter)
        [(#\a #\e #\i #\o #\u) (fill 255 204 0)]
        [else                  (fill 255)])
      ; Draw the letter
      (text letter x y)
      ; Increment the counter
      (++ counter))))
