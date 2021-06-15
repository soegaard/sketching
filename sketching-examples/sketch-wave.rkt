#lang sketching
; https://processing.org/examples/additivewave.html

(define x-spacing 8) ; How far apart should each horizontal location be spaced
(define w         1) ; Width of entire wave
(define max-waves 3) ; total # of waves to add together

(define theta 0.0)
(define amplitude (make-vector max-waves 0.0))
(define dx        (make-vector max-waves 0.0)) ; Value for incrementing X, to be calculated as a function of period and xspacing
(define y-values  (make-vector max-waves 0.0)) ; Using an array to store height values for the wave (not entirely necessary)

(define ref vector-ref)
(define set vector-set!)
(define len vector-length)

(define (setup)
  (size 640 360)
  (frame-rate 60)
  ; colorMode(RGB, 255, 255, 255, 100);
  (set! w (+ width 16))
  (for ([i (in-range 0 max-waves)])
    (define period (random 100 300))
    (set amplitude i (random 10 30))
    (set dx        i (* (/ (* 2 3.1415) period) x-spacing)))  
  (set! y-values (make-vector (/ w x-spacing) 0.0)))

(define (draw)
  (background 0)
  (calc-wave)
  (render-wave))


(define (calc-wave)
  ;; Increment theta (try different values for 'angular velocity' here
  (set! theta (+ theta 0.02))

  ;; Set all height values to zero
  (for ([i (len y-values)])
    (set y-values i 0))
  
  ;; Accumulate wave height values
  (for ([j (in-range max-waves)])
    (define x theta)
    (for ([i (len y-values)])
      ;; Every other wave is cosine instead of sine
      (define f (if (even? j) sin cos))
      (set y-values i (+ (ref y-values i) (* (f x) (ref amplitude j))))
      (set! x (+ x (ref dx j))))))

(define (render-wave)
  ;; A simple way to draw the wave with an ellipse at each location
  (no-stroke)
  (fill 255 128)
  (ellipse-mode 'center)
  (for ([x (len y-values)])
    (ellipse (* x x-spacing) (+ (/ height 2) (ref y-values x)) 16 16)))
