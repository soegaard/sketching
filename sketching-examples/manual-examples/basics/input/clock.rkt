#lang sketching
; https://github.com/processing/processing-docs/blob/master/content/examples/Basics/Input/Clock/Clock.pde
; Clock. 
;  
;  The current time can be read with the second(), minute(), 
;  and hour() functions. In this example, sin() and cos() values
;  are used to set the position of the hands.

(define w 640)
(define h 360)

(define cx (/ w 2))
(define cy (/ h 2))

(define radius         (/ (min w h) 2))
(define seconds-radius (* 0.72 radius))
(define minutes-radius (* 0.60 radius))
(define hours-radius   (* 0.50 radius))
(define clock-diameter (* 1.80 radius))

(define (setup)
  (size 640 360)
  (frame-rate 1)
  (stroke 255))

(define (draw)
  (background 0)

  ; draw clock background
  (fill 80)
  (no-stroke)
  (ellipse cx cy clock-diameter clock-diameter)

  ; Angles for sin() and cos() start at 3 o'clock;
  ; subtract pi/2 to make them start at the top
  (define s (- (remap (second)                        0 60 0 2pi) pi/2))
  (define m (- (remap (minute)                        0 60 0 2pi) pi/2))
  (define h (- (remap (+ (hour) (norm (minute) 0 60)) 0 24 0 2pi) pi/2))

  ; Draw the hands of the clock  
  (stroke 255)
  (stroke-weight 1)
  (line cx cy (+ cx (* (cos s) seconds-radius)) (+ cy (* (sin s) seconds-radius)))
  (stroke-weight 2)
  (line cx cy (+ cx (* (cos m) minutes-radius)) (+ cy (* (sin m) minutes-radius)))
  (stroke-weight 4)
  (line cx cy (+ cx (* (cos h) hours-radius))   (+ cy (* (sin h) hours-radius)))
    
  ; Draw the minute ticks
  (stroke-weight 2)
  (for ([a (in-range 0 360 6)])
    (define angle (radians a))
    (define x (+ cx (* (cos angle) seconds-radius)))
    (define y (+ cy (* (sin angle) seconds-radius)))
    (point x y)))
