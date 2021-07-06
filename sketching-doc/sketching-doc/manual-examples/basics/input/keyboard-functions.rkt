#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/KeyboardFunctions/KeyboardFunctions.pde
; Keyboard Functions.

; "Color Typewriter" is an art installation by John Maeda.
;   https://www.ntticc.or.jp/en/archive/works/color-typewriter/
; Martin Gomez were inspired and wrote a Processing version,
; which in modified form became an example in the Processing manual.
; That example was then ported to Sketching.

; Click on the window to give it focus and press the letter keys to type colors. 
; The keyboard function on-key-pressed is called whenever a key is pressed.
; The functions on-key-released is called when a key is released.

(define max-height 40)
(define min-height 20)
(define letter-height max-height)
(define letter-width  20)

(define x (- letter-width)) ; position of the next letter
(define y 0)

(define new-letter? #t)

(define n      26)                ; There are 26 letters in the alphabet
(define colors (make-vector n 0)) ; each key has a color 

(define (setup)
  (size 640 360)
  (no-stroke)
  (color-mode 'hsb n)
  (background (/ n 2))
  ; set hue for each key
  (for ([i (in-range n)])
    (:= colors i (color i n n))))

(define (draw)
  (when (= frame-count 0)
    (no-stroke)
    (background "cyan"))
  (when new-letter?
    (cond
      [(= letter-height max-height)
       (rect x y letter-width letter-height)]
      [else
       (define y1 (+ y min-height))
       (rect x y1 letter-width letter-height)
       (fill "cyan")
       (rect x (- y1 min-height) letter-width letter-height)])
    (:= new-letter? #f)))


(define (on-key-pressed)
  ; tell draw we have a new letter
  (:= new-letter? #t)
  ; 
  (cond
    [(and (char? key)
          (<= (int #\a) (int (char-downcase key)) (int #\z)))
     (define key-index (- (int (char-downcase key)) (int #\a)))
     (fill (colors.ref key-index))
     (:= letter-height
         (if (char-upper-case? key) max-height min-height))]
    [else
     (fill 0)
     (:= letter-height 10)])

  ; update letter position
  (:= x (+ x letter-width))

  ; wrap horizontally
  (when (> x (- width letter-width))
    (:= x 0)
    (+= y max-height))

  ; wrap vertically
  (when (> y (- height letter-height))
    (:= y 0)))
